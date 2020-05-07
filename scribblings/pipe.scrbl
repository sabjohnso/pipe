#lang scribble/manual
@require[@for-label[pipe racket/base]
                              scribble/eval
                              pipe]

@title{pipe}
@author{"Samuel B. Johnson"}

@defmodule[pipe]

@defform[(-> expr application-form ...)
             #:grammar
             ([application-form (procedure-expr expr ...)]
              [procedure-expr procedure?])]

@defform[(->> expr application-form ...)
             #:grammar
             ([application-form (procedure-expr expr ...)]
              [procedure-expr procedure?])]

@defform[(>- id -> expr application-form ...)
             #:grammar
             ([application-form (procedure-expr expr ...)]
              [procedure-expr procedure?])]

@defform[(>>- id -> expr application-form ...)
             #:grammar
             ([application-form (procedure-expr expr ...)]
              [procedure-expr procedure?])]
