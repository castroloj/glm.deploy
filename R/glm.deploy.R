# glm.deploy:  Generates the source code of the scoring algorithm (predict) of a GLM object to C or JAVA to deploy/operationalize it outside R.
#
# Copyright (c) 2018, Oscar J. Castro-Lopez, Ines F. Vega-Lopez
#
# This file is part of the glm.deploy package for R.
#
# The glm.deploy: package is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# The glm.deploy: package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details (http://www.gnu.org/licenses/).
######################################################################################

#' @title glm.deploy
#' @description the glm.deploy package allows to generate source code from glm objects to deploy/operationalize the scoring/predict functions outside R.
#' glm.deploy is used to generate the scoring functions of a trained GLM model in C or JAVA.
#' It should not be called directly
glm.deploy <- function(model, filename = NULL, language) {
  if (!inherits(model, "glm"))
    stop("ERROR: Not a glm object")

  allcoefs = dummy.coef(model)
  Fields = c()
  Coefficients = c()
  Datatypes = attributes(model$terms)$dataClasses
  Types = c()
  Arguments = attributes(model$terms)$dataClasses
  Factors = c()
  Factorsname = c()
  Intercept_label = paste0("", names(attributes(model$terms)$dataClasses[1]))
  i = 1
  j = 1
  hasfactor = FALSE
  for (x in 1:length(allcoefs)) {
    if (length(allcoefs[[x]]) == 1) {
      Fields[i] = names(allcoefs[x])
      Factors[i] = NA
      Factorsname[i] = NA
      Coefficients[i] = allcoefs[[x]]
      Types[i] = unname(Datatypes[grep(names(allcoefs[x]), names(Datatypes))])
      i = i + 1
      j = j + 1
    } else{
      ##If data is factor
      hasfactor = TRUE
      j = j + 1
      for (y in 1:length(allcoefs[[x]])) {
        Fields[i] = paste0(names(allcoefs[x]), names(allcoefs[[x]][y]))
        Factors[i] = names(allcoefs[[x]][y])
        Factorsname[i] = names(allcoefs[x])
        Coefficients[i] = allcoefs[[x]][y]
        Types[i] = unname(Datatypes[grep(names(allcoefs[x]), names(Datatypes))])
        i = i + 1
      }
    }
  }
  #Substitute invalid characters for variable names, and transform variable names to lower case
  names(Arguments) = tolower(gsub("\\.", "_", names(Arguments)))
  Fields = tolower(gsub("\\.", "_", Fields))
  Intercept_label = tolower(gsub("\\.", "_", Intercept_label))
  #UTF8 ENCODING
  Fields = enc2utf8(Fields)
  Intercept_label = enc2utf8(Intercept_label)
  Arguments = enc2utf8(Arguments)

  tbl = data.frame(
    Field = Fields,
    Coefficient = Coefficients,
    Type = Types,
    Factor = Factors,
    Factorname = Factorsname
  )
  if (is.null(filename)) {
    filename = ""
  }
  glmdeploy_cpp(
    tbl,
    Arguments,
    model$family$family,
    model$family$link,
    Intercept_label,
    hasfactor,
    filename,
    language
  )
}

#' @name glm2c
#' @title glm to c source code to deploy/operationalize outside R
##' @description glm2c is used to generate the scoring functions of a trained GLM model in the c language.\cr
##' It generates a file with two functions:\cr
##' 1) The equivalent to predict(type="response").\cr
##' 2) The equivalent to predict(type="link").
##' @param model A trained object of class "glm".
##' @param filename The name of the file to save, if NULL the default is "glm_" plus the (intercept) variable name.
##' @author Oscar J. Castro-Lopez, Ines F. Vega-Lopez
##' @examples
#' glm2c(glm(Y ~ ., family = binomial(logit), data=data), "MyFileName")
glm2c <- function(model, filename = NULL) {
  glm.deploy(model, filename, 0)
}

#' @name glm2java
#' @title glm to java source code to deploy/operationalize outside R
##' @description glm2java is used to generate the scoring functions of a trained GLM model in the Java language.\cr
##' It generates a class with two functions:\cr
##' 1) The equivalent to predict(type="response").\cr
##' 2) The equivalent to predict(type="link").
##' @param model A trained object of class "glm".
##' @param filename The name of the file to save, if NULL the default is "glm_" plus the (intercept) variable name.
##' @author Oscar J. Castro-Lopez, Ines F. Vega-Lopez
##' @examples
#' glm2java(glm(Y ~ ., family = binomial(logit), data=data), "MyFileName")
glm2java <- function(model, filename = NULL) {
  glm.deploy(model, filename, 1)
}
