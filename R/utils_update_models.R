# This file is part of the R package "aifeducation".
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as published by
# the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>


#'@title Update values from 1.0.2 to 1.1.0
#'@description Function can be used to update old values to the new version used in version 1.1.0 and higher.
#'If the values does not require an update it will return the original value.
#'@param value Any object to check and replace if necessary. Please not that this functions checks and updates only strings.
#'@returns Returns the new `string` or the input to the function.
#' @family Utils Update Developers
#' @keywords internal
#' @noRd
update_values_to_new_1.1.0=function(value){

  if(is.null_or_na(value)){
    return(value)
  } else if (is.character(value) & length(value)==1){
    #column 1 represent the old values
    #column 2 represent the new values
    data=c(
      "last","Last",
      "middle","Middle",
      "first","First",
      "gru","GRU",
      "lstm","LSTM",
      "adam","Adam",
      "adamw","AdamW",
      "sgd","SGD",
      "rmsprop","RMSprop",
      "dense","Dense",
      "elu","ELU",
      "leakyrelu","LeakyReLU",
      "relu","ReLU",
      "gelu","GELU",
      "sigmoid","Sigmoid",
      "tanh","Tanh",
      "prelu","PReLU",
      "multihead","MultiHead",
      "fourier","Fourier",
      "cls","CLS",
      "average","Average",
      "mean","Mean"
    )

    value_table=matrix(
      data=data,
      ncol=2,
      byrow=TRUE
    )

    ind=which(value_table[,1]==value)
    if(length(ind)==1){
      new_value=value_table[ind,2]
      return(new_value)
    } else {
      return(value)
    }
  } else {
    return(value)
  }


}
