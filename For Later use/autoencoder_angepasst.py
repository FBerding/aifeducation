class DenseAutoencoder_with_Mask_PT(torch.nn.Module):
    def __init__(self, features_in, features_out, times, noise_factor, pad_value, orthogonal_method, te_n_layers=3):
        super().__init__()
        self.features_in = features_in
        self.times = times
        self.features_out = features_out
        self.noise_factor = noise_factor
        self.n_layers = te_n_layers
        self.difference = self.features_in - self.features_out
        
        # Feature-Größen berechnen
        dims = []
        for i in range(self.n_layers + 1):
            fraction = i / self.n_layers
            dim = math.ceil(self.features_in - self.difference * fraction)
            dims.append(dim)
            
        # Residual-Verbindung als Linear-Layer ohne Bias
        self.residual = torch.nn.Linear(features_in, features_out, bias=False)
        torch.nn.init.uniform_(self.residual.weight, 0.0, 1.0)
        
        # Encoder Dense-Layer
        self.encoder_layers = torch.nn.ModuleList()
        self.encoder_lns = torch.nn.ModuleList()
        # Separate Liste für die eigenen Decoder-Biases
        self.decoder_biases = torch.nn.ParameterList()
        self.decoder_lns = torch.nn.ModuleList()
        self.activation = {}
        
        for i in range(self.n_layers):
            out_d = dims[i+1]
            in_d = dims[i]
            
            # Encoder Layer (enthält eigenen Weight und Encoder-Bias)
            layer = torch.nn.Linear(in_features=in_d, out_features=out_d, bias=True)
            torch.nn.init.orthogonal_(layer.weight)
            torch.nn.init.zeros_(layer.bias)
            
            if orthogonal_method != "None":
                torch.nn.utils.parametrizations.orthogonal(module=layer, name="weight", orthogonal_map=orthogonal_method)
                
            self.encoder_layers.append(layer)
            
            # Eigener Decoder-Bias: Da der Decoder rückwärts läuft und die Gewichte transponiert, 
            # ist die Ausgangsdimension des entsprechenden Decoder-Schritts gleich der Eingangsdimension `in_d` des Encoders.
            dec_bias = torch.nn.Parameter(torch.zeros(in_d))
            self.decoder_biases.append(dec_bias)
            
            if i > 0 and i < self.n_layers - 1:
              lns_name="LayerNorm"
            else:
              lns_name="None"
            #Normalizer
            self.encoder_lns.append(
              get_layer_normalization(
                name=lns_name,
                times=self.times, 
                features=in_d,
                pad_value=0.0,
                eps=1e-6
              )
            )
            self.decoder_lns.append(
              get_layer_normalization(
                name=lns_name,
                times=self.times, 
                features=out_d,
                pad_value=0.0,
                eps=1e-6
              )
            )
            
        # Padding Layer
        if pad_value != 0:
            self.switch_pad_value_start = layer_switch_pad_values(pad_value_old=pad_value, pad_value_new=0)
            self.switch_pad_value_final = layer_switch_pad_values(pad_value_old=0, pad_value_new=pad_value)
        else:
            self.switch_pad_value_start = None
            self.switch_pad_value_final = None

    def forward(self, x, encoder_mode=False):
        if self.switch_pad_value_start is not None:
            x = self.switch_pad_value_start(x)
            
        if encoder_mode:
            latent_space = self.encoder(x)
            if self.switch_pad_value_start is not None:
                latent_space = self.switch_pad_value_final(latent_space)
            return latent_space
        else:
            latent_space = self.encoder(x)
            target = self.decoder(
                output_mask=self.get_mask(x, self.features_in),
                y=latent_space
            )
            if self.switch_pad_value_start is not None:
                target = self.switch_pad_value_final(target)
            return target, latent_space   

    def encoder(self, x):
        mask = self.get_mask(x, self.features_in)
        if self.training:
            x_noise = x + self.add_noise(x)
            x_noise = ~mask * x_noise
            y = x_noise
        else:
            x_noise = x
            y = x
            
        for idx, layer in enumerate(self.encoder_layers):
            if idx > 0 and idx < self.n_layers - 1:
                y,_ =  self.encoder_lns[idx](y,torch.squeeze(mask,dim=2))
                y = layer(y)
                y = torch.nn.functional.gelu(y)
            else:
                y = layer(y)
            y = y * ~mask
            
        y = self.residual(x_noise) + y
        #y = torch.nn.functional.normalize(y, p=2, dim=-1)
        y = y * ~mask
        return y

    def decoder(self, output_mask, y):
        latent_space = y
        
        # Umkehrung der Layer und Biases für den Decoder
        reversed_layers = list(reversed(self.encoder_layers))
        reversed_dec_biases = list(reversed(self.decoder_biases))
        decoder_lns_rev=list(reversed(self.decoder_lns))
        
        for idx, layer in enumerate(reversed_layers):
            w = layer.weight
            b_dec = reversed_dec_biases[idx] # Der eigene Decoder-Bias für diesen Schritt
            
            if idx > 0 and idx < self.n_layers - 1:
                y,_ =  decoder_lns_rev[idx](y,torch.squeeze(output_mask,dim=2))
                # Der eigene Bias wird direkt in die lineare Operation übergeben (y * w.T + b_dec)
                y = torch.nn.functional.linear(y, weight=w.t(), bias=b_dec)
                y = torch.nn.functional.gelu(y)
            else:
                y = torch.nn.functional.linear(y, weight=w.t(), bias=b_dec)
            y = y * ~output_mask
            
        y = torch.nn.functional.linear(latent_space, weight=self.residual.weight.t()) + y  
        return y
      
    def get_mask(self, x, features):
        time_sums = torch.sum(x, dim=2, keepdim=True)
        mask = (time_sums == 0)
        mask = mask.expand((x.size(0), x.size(1), 1))
        return mask.detach()

    def add_noise(self, x):
        noise = self.noise_factor * torch.rand(size=x.size(), device=x.device, dtype=x.dtype)
        return noise.detach()
