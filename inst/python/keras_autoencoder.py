import tensorflow as tf
import tensorflow.keras as keras
from keras import backend as K
import math



@keras.saving.register_keras_serializable(package="aifeducation")
class NoiseLayer(keras.layers.Layer):
  def __init__(self, mean, stddev, noise_factor, **kwargs):
    super().__init__(**kwargs)
    self.supports_masking=True
    self.noise_factor=noise_factor
    self.mean=mean
    self.stddev=stddev
    
  def call(self,inputs,training=None,mask=None):
    if training:
      if mask is None:
        noise=self.noise_factor*tf.random.normal(shape=tf.shape(inputs),mean=self.mean,stddev=self.stddev)
      else:
        noise=self.noise_factor*tf.random.normal(shape=tf.shape(inputs),mean=self.mean,stddev=self.stddev)
        noise=tf.math.multiply(x=noise,y=tf.expand_dims(tf.cast(mask, tf.float32),2))
      return inputs+noise
    else:
      return inputs
  def get_config(self):
    config=super().get_config()
    config.update({
      "noise_factor": self.noise_factor,
      "mean": self.mean,
      "stddev": self.stddev,
    })
    return config
  
@keras.saving.register_keras_serializable(package="aifeducation")
class PaddingLayer(keras.layers.Layer):
  def __init__(self, **kwargs):
    super().__init__(**kwargs)

  def call(self,inputs,mask=None):
    if mask is None:
      return inputs
    else:
      x=tf.math.multiply(x=inputs,y=tf.expand_dims(tf.cast(mask, tf.float32),2))
    return x

def LSTMAutoencoder_with_Mask_TF(times,features_in,features_out,noise_factor):
  difference=features_in-features_out
  
  model=tf.keras.Sequential()
  model.add(keras.Input(shape=(times,features_in)))
  model.add(keras.layers.Masking(mask_value=0.0,name="masking"))
  model.add(NoiseLayer(mean=0.0,stddev=1.0,noise_factor=noise_factor,name="noise_layer"))
  model.add(keras.layers.LSTM(units=math.ceil(features_in-difference*(1/2)),return_sequences=True,name="encoder_1"))
  model.add(keras.layers.LSTM(units=features_out,return_sequences=True,name="latent_space"))
  model.add(PaddingLayer(name="latent_space_output"))
  model.add(keras.layers.LSTM(units=math.ceil(features_in-difference*(1/2)),return_sequences=True,name="decoder_1"))
  model.add(keras.layers.LSTM(units=features_in,return_sequences=True,name="reconstructed_seq"))
  model.add(PaddingLayer(name="padding_layer"))
  
  return model
  




