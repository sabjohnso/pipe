#lang scribble/manual
@require[@for-label[(prefix-in pipe: pipe) racket/base]]

@title{pipe}
@author{sbj}

@defmodule[pipe]

@defform[(-> input application-form ...)
             #:grammar
             ([input expr]
              [application-form (procedure-expr argument-expr ...)]
              [procedure-expr procedure?]
              [agurment-expr expr])]
              


