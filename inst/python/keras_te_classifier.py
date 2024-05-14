import tensorflow as tf
import keras

def extract_sample_weight(inputs, labels):
    weight = inputs.pop("sample_weights")
    return inputs, labels, weight

@keras.saving.register_keras_serializable(package="aifeducation")
class TransformerEncoder(keras.layers.Layer):
  def __init__(self, embed_dim, dense_dim, num_heads,dropout_rate, **kwargs):
    super().__init__(**kwargs)
    self.embed_dim=embed_dim
    self.dense_dim=dense_dim
    self.num_heads=num_heads
    self.dropout_rate=dropout_rate
    
    self.attention=keras.layers.MultiHeadAttention(
      num_heads=num_heads, key_dim=embed_dim)
    self.dense_proj=keras.Sequential(
      [keras.layers.Dense(dense_dim,activation="gelu"),
      keras.layers.Dense(embed_dim),])
    self.layernorm_1=keras.layers.LayerNormalization()
    self.layernorm_2=keras.layers.LayerNormalization()
    self.dropout=keras.layers.Dropout(rate=dropout_rate)

  def call(self,inputs,mask=None,training=False):
    if mask is not None:
      mask=mask[:,tf.newaxis,:]
    attention_output=self.attention(
      inputs,inputs,attention_mask=mask,training=training)
    attention_output=self.dropout(attention_output,training=training)
    
    proj_input=self.layernorm_1(inputs+attention_output,training=training)
    proj_output=self.dense_proj(proj_input)
    return self.layernorm_2(proj_input+proj_output,training=training)
  
  def compute_mask(self,inputs,mask=None):
    return mask
  
  def get_config(self):
    config=super().get_config()
    config.update({
      "embed_dim": self.embed_dim,
      "num_heads": self.num_heads,
      "dense_dim": self.dense_dim,
      "dropout_rate": self.dropout_rate,
    })
    return config
      
@keras.saving.register_keras_serializable(package="aifeducation")
class FourierTransformation(keras.layers.Layer):
  def __init__(self,**kwargs):
    super().__init__(**kwargs)

  def call(self,inputs):
    return tf.math.real(tf.signal.fft2d(tf.cast(x=inputs,dtype=tf.complex64)))

@keras.saving.register_keras_serializable(package="aifeducation")
class FourierEncoder(keras.layers.Layer):
  def __init__(self, dense_dim, dropout_rate, **kwargs):
    super().__init__(**kwargs)
    self.dense_dim=dense_dim
    self.dropout_rate=dropout_rate
   
    self.layernorm_1=keras.layers.LayerNormalization()
    self.layernorm_2=keras.layers.LayerNormalization()
    
    self.dropout=keras.layers.Dropout(rate=dropout_rate)

  def build(self,input_shape):
    self.features=input_shape[-1]
    self.attention=FourierTransformation()
    self.dense_proj=keras.Sequential(
      [keras.layers.Dense(self.dense_dim,activation="gelu"),
      keras.layers.Dense(self.features),])
    
  def call(self, inputs,training=False):
    attention_output=self.attention(inputs)
    attention_output=self.dropout(attention_output,training=training)
    proj_input=self.layernorm_1(attention_output,training=training)
    proj_output=self.dense_proj(proj_input)
    return self.layernorm_2(proj_input+proj_output,training=training)
    
  def get_config(self):
    config=super().get_config()
    config.update({
      "dense_dim": self.dense_dim,
      "dropout_rate": self.dropout_rate,
    })
    return config
  
@keras.saving.register_keras_serializable(package="aifeducation")      
class AddPositionalEmbedding(keras.layers.Layer):
  def __init__(self, sequence_length, **kwargs):
    super().__init__(**kwargs)
    self.sequence_length=sequence_length
    
  def build(self,input_shape):
    self.output_dim=input_shape[-1]
    self.position_embedding=keras.layers.Embedding(
      input_dim=self.sequence_length,
      output_dim=self.output_dim)
  
  def call(self, inputs, mask=None, training=False):
    len=tf.shape(inputs)[1]
    positions=tf.range(start=0,limit=len,delta=1)
    embedded_positions=self.position_embedding(positions)
    embedded_positions_masked=tf.where(
      condition=(inputs!=0),
      x=embedded_positions,
      y=tf.zeros(shape=tf.shape(embedded_positions))
    )
    return inputs+embedded_positions_masked
  
  def compute_mask(self,inputs,mask=None):
    return mask
    
  def get_config(self):
    config=super().get_config()
    config.update({
      "sequence_length": self.sequence_length,
    })
    return config

  
#Balanced Accuracy Metric
@keras.saving.register_keras_serializable(package="aifeducation")
class BalancedAccuracy(tf.keras.metrics.Metric):
  def __init__(self, n_classes, name='balanced_accuracy', **kwargs):
    super().__init__(name=name, **kwargs)
    self.n_classes=n_classes
    self.assignments=tf.Variable(tf.zeros(shape=[self.n_classes,self.n_classes]))
    
  def update_state(self, y_true, y_pred,sample_weight=None):
    if self.n_classes==2:
      classes_y_true=tf.math.argmax(input=tf.concat(values=(y_true,1-y_true),axis=1),axis=1)
      classes_y_pred=tf.math.argmax(input=tf.concat(values=(y_pred,1-y_pred),axis=1),axis=1)
    else:
      classes_y_true = tf.math.argmax(input=y_true,axis=1)
      classes_y_pred = tf.math.argmax(input=y_pred,axis=1)
    
    tmp_assignments=tf.math.confusion_matrix(
      labels=classes_y_true,
      predictions=classes_y_pred,
      num_classes=self.n_classes,
      weights=None,
      dtype=tf.dtypes.float32,
      name=None)

    self.assignments.assign_add(tmp_assignments)

  def result(self):
    bacc=tf.math.reduce_sum(tf.linalg.diag_part(self.assignments)/tf.math.reduce_sum(self.assignments,axis=1))/self.n_classes
    return bacc

  def reset_state(self):
     self.assignments.assign(tf.zeros(shape=[self.n_classes,self.n_classes]))
     
  def get_config(self):
    config=super().get_config()
    config.update({
      "n_classes": self.n_classes,
    })
    return config





    
    
