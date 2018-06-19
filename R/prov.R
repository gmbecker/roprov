##' @import CodeDepends fastdigest

##' @exportClass ProvStoreDF
##' @rdname ProvStoreDF
setClass("ProvStoreDF",
         representation(provdata = "data.frame",
                        hashprefix = "character"),
         validity = function(object) {
    provdfrow = provdf()
    objpd = provdata(object)
    if(!identical(names(objpd), names(provdfrow)))
        stop("wrong column names on provdfrow: ", paste(names(objpd), collapse = ","))
    stopifnot(identical(sapply(objpd, class), sapply(provdfrow, class)),
              nzchar(hashprefix(object)))
    TRUE
})

                  


## defines the columns (including order) that
## a provenance store dataframe is expected
## to have.
provdf = function(outputvar = character(),
                  outputvarhash = character(),
                  outputvarclass = character(),
                  inputvar = character(),
                  inputvarhash = character(),
                  inputvarclass = character(),
                  agent = character(),
                  code = character(),
                  codehash = if(length(code)) vapply(code, fastdigest, character(1)) else character()) {
    data.frame(outputvar = outputvar,
               outputvarhash = outputvarhash,
               outputvarclass = outputvarclass,
               inputvar = inputvar,
               inputvarhash = inputvarhash,
               inputvarclass = inputvarclass,
               agent = agent,
               code = code,
               codehash = codehash,
               stringsAsFactors = FALSE)
}




## XXX TODO make a generic and support ScriptInfo
##' alloutputs
##' @description Get all output variables (outputs + updates) from a ScriptNodeInfo object
##' @return a character vector of all variables which were created or modified
##' by the expression corresponding to \code{x}
##' @export

cback1 = function(invar, prevouts, prevouthashes, prevoutclasses) {
    
    revprev = rev(prevouts)
    ind = match(invar, revprev)
    if(is.na(ind)) stop("input variable ", invar, "not found as previous output. Incomplete history?")
    realind = length(prevouts) - ind + 1 # 1-based indices
    print(realind)
    list(hash = prevouthashes[realind],
         class = prevoutclasses[realind])
}

#' Generate a provenance store from hashed output history
#' @description Generate provenance of a single variable from a
#'     history of hashed outputs.
#' @param outvar character(1). A single output variable
#' @param outvarhash character(1). The hash of the value of
#'     \code{outvar}.
#' @param outvarclass character(1). The (top level) class of the
#'     output variable
#' @param invars A vector of input variables for \code{outvar}.
#' @param prevouts character. A vector of previous output variables
#' @param prevouthashes character. A vector of previous output hashes
#'     corresponding to \code{prevouts}
#' @param prevoutclasses character. A vector of (top level) classes
#'     corresponding to \code{prevouts}
#' @param code The code corresponding with the generation of
#'     \code{outvar}
#' @return A ProvStoreDF object
#' @export
provFromHistory = function(outvar, outvarhash, outvarclass, invars,
                           prevouts, prevouthashes, prevoutclasses, code) {
    lsts = lapply(invars, cback1, prevouts = prevouts,
                  prevouthashes = prevouthashes,
                  prevoutclasses = prevoutclasses)

    invarhashes = sapply(lsts, function(x) x$hash)
    invarclasses = sapply(lsts, function(x) x$class)
    makeProvStore(invarhashes = invarhashes, invarclasses = invarclasses,
                  outvarhashes = outvarhash, outvarclasses = outvarclass,
               code = code, invars = invars, outvars = outvar)
}
        
getUser = function() system("whoami", intern=TRUE)

.fixcode = function(code) paste(code, collapse="\n")


##' @title ProvStoreDF constructors and class
##' @description Functions to create a valid ProvStore object from raw data
##' about evaluation history (including hashes of input and output variable
##' values).
##'
##' @details makeProvStore expects information about a single code unit. i.e.,
##' all input values will be counted as inputs for all outputs. For
##' makeProvStore the number of inputs and outputs need not match, and the
##' appropriate replication on inputs will happen automatically.
##'
##' ProvStoreDF is a direct constructor and thus expects the replciation of
##' inputs for multiple outputs to have already occurred. 
##' 
##' @param invarhashes character. Hashes of values for input variables
##'     (or NULL).
##' @param outvarhashes character. Hashes of values for output
##'     variables (or NULL).
##' @param code character. The code associated with the input and
##'     output variables in question.
##' @param codehash character. The hash of the (parsed and deparsed)
##'     code.
##' @param invars character. The set of input variables (symbols) to
##'     \code{code}
##' @param outvars character. The output variable(s) (symbols) to
##'     \code{code}.
##' @param agent character. a string identifying the user. Defaults to
##'     the output of a \code{whoami} system call.
##' @param hashprefix character. A prefix to append to the hashes if
##'     not present already to make them more self-describing.
##' @rdname ProvStoreDF
##' @export
makeProvStore = function(invarhashes = NULL, outvarhashes = NULL, code = NULL,
                      codehash = fastdigest(code),
                      invars = names(invarhashes),
                      outvars = names(outvarhashes),
                      invarclasses = character(),
                      outvarclasses = character(),
                      agent = getUser(),
                      hashprefix = "SpookyV2"
                      ) {
    if(length(invarhashes) == 0 && length(outvarhashes) == 0) {
        ## empty
        return(ProvStoreDF( hashprefix = hashprefix))
    }
    ##  if(is.null(invarhashes) || is.null(outvarhashes) || is.null(code))
      if(is.null(outvarhashes) || is.null(code))
        stop("One or more required argument was not specified",
             "(required args: otvarhashes, code)")
    
    if(length(invars) != length(invarhashes) || sum(nzchar(invars)) != length(invarhashes))
        stop(sprintf(paste("Number of non-empty input variable names",
                           "(%d) does not match number of input var hashes (%d)"),
                     nzchar(invars), length(invarhashes)))
    if(length(outvars) != length(outvarhashes) ||
       sum(nzchar(outvars)) != length(outvarhashes))
        stop(sprintf(paste("Number of non-empty output variable names",
                       "(%d) does not match number of output var hashes (%d)"),
                     nzchar(outvars), length(outvarhashes)))
    if(length(invars) == 0) {
        invars = ""
        invarhashes = ""
        invarclasses = ""
    }
        
    nouts = length(outvars)
    ninputs = length(invars)
    outvars = rep(outvars, ninputs)
    outvarhashes = rep(outvarhashes, ninputs)
    outvarclasses = rep(outvarclasses, ninputs)
    invarhashes = rep(invarhashes, each = nouts)
    invars = rep(invars, each = nouts)
    invarclasses = rep(invarclasses, each = nouts)
    if(is(code, "list"))
        code = sapply(code, .fixcode)
    else
        code = .fixcode(code)
    
    code = rep(code, times = nouts * ninputs)
    ## cat("\noutvars: ", outvars, "\ninvars: ", invars,
    ##     "\ncode(nchar): ", code,"(", nchar(code), ")", "\ncodehash: ", codehash,
    ##     "\nagent: ", agent, sep = " ")
    ProvStoreDF(outputvar = outvars, outputvarhash = outvarhashes,
                outputvarclass = outvarclasses,
                inputvar = invars, inputvarhash = invarhashes,
                inputvarclass = invarclasses,
                agent = agent, code = code,
                codehash = codehash,
                hashprefix = hashprefix)
}




##' @rdname ProvStoreDF
##' @aliases ProvStoreDF
##' @param df data.frame. Optional. An already constructed data.frame
##'     do use as the provdata of the constructed \code{ProvStoreDF}
##'     object.
##' @export
ProvStoreDF = function(outputvar = character(),
                       outputvarhash = character(),
                       outputvarclass = character(),
                  inputvar = character(),
                  inputvarhash = character(),
                  inputvarclass = character(),
                  agent = character(),
                  code = character(),
                  codehash = vapply(code, fastdigest, character(1)),
                  hashprefix = "SpookyV2",
                  df = NULL) {
    
    if(is.null(df)) {
        df = provdf(outputvar = outputvar,
                    outputvarhash = outputvarhash,
                    outputvarclass = outputvarclass,
               inputvar = inputvar,
               inputvarhash = inputvarhash,
               inputvarclass = inputvarclass,
               agent = agent,
               code = code,
               codehash = codehash)
    }
    new("ProvStoreDF", provdata = df, 
        hashprefix = hashprefix)
}

##' @title Accessors
##' @rdname accessors
##' @param obj The object.
##' @export
setGeneric("outputvars", function(obj) standardGeneric("outputvars"))
setMethod("outputvars", "ProvStoreDF", function(obj) provdata(obj)$outputvar)

##' @rdname accessors
##' @export
##' @aliases outputvarhashes
setGeneric("outputvarhashes", function(obj) standardGeneric("outputvarhashes"))
setMethod("outputvarhashes", "ProvStoreDF", function(obj) provdata(obj)$outputvarhash)

##' @rdname accessors
##' @export
##' @aliases outputvarclasses
setGeneric("outputvarclasses", function(obj) standardGeneric("outputvarclasses"))
setMethod("outputvarclasses", "ProvStoreDF", function(obj) provdata(obj)$outputvarclass)


##' @rdname accessors
##' @export
##' @aliases inputvars
setGeneric("inputvars", function(obj) standardGeneric("inputvars"))
setMethod("inputvars", "ProvStoreDF", function(obj) provdata(obj)$inputvar)

##' @rdname accessors
##' @export
##' @aliases inputvarhashes
setGeneric("inputvarhashes", function(obj) standardGeneric("inputvarhashes"))
setMethod("inputvarhashes", "ProvStoreDF", function(obj) provdata(obj)$inputvarhash)

##' @rdname accessors
##' @export
##' @aliases inputvarclasses
setGeneric("inputvarclasses", function(obj) standardGeneric("inputvarclasses"))
setMethod("inputvarclasses", "ProvStoreDF", function(obj) provdata(obj)$inputvarclass)

##' @rdname accessors
##' @export
##' @aliases provdata
setGeneric("provdata", function(obj) standardGeneric("provdata"))
setMethod("provdata", "ProvStoreDF", function(obj) obj@provdata)


##' @rdname accessors
##' @export
##' @aliases hashprefix
setGeneric("hashprefix", function(obj) standardGeneric("hashprefix"))
setMethod("hashprefix", "ProvStoreDF", function(obj) obj@hashprefix)



setAs("ProvStoreDF", "data.frame", function(from) {
    df = provdata(from)
    df$hashprefix = hashprefix(from)
    df
})

setAs("data.frame", "ProvStoreDF", function(from) {
  
    
    if(!is.null(from$hashprefix)) {
        hashprefix = from$hashprefix
        from$hashprefix = NULL
        if(length(unique(hashprefix)) > 1)
            stop("data.frame contains hashprefix column with more than one unique value")
    }
    ProvStoreDF(df = from, hashprefix = hashprefix)
})    

##' @import igraph
##' @title Create the full (multiple values per variable) provenance
##'     graph for a cache
##' @description This generates and returns the \emph{full} provenance
##'     graph reflecting the information stored in the cache or
##'     data.frame specified. This can include the same variable
##'     multiple times if the corresponding expression is run with
##'     different inputs.
##' @param cacheobj a CachingEngine or CodeSetCache object. Ignored if
##'     \code{df} is set.
##' @param df ProvStoreDF (or appropriately formatted
##'     data.frame). Generally \code{cacheobj} should be specified and
##'     the default of \code{cacheobj$provstore} should be used.
##' @return an igraph object representing the provenance graph
##' @docType methods
##' @export
setGeneric("fullprovgraph", function(provstore) standardGeneric("fullprovgraph"))

setMethod("fullprovgraph", "ProvStoreDF",
          function(provstore) fullprovgraph(provdata(provstore)))

setMethod("fullprovgraph", "data.frame",
          function(provstore) {
    provcolnames = names(provdf())
    
    if(!identical(names(provstore)[seq(along = provcolnames)],
                  provcolnames)) {
        msg = sprintf(paste("The first %d columns of a provenance",
                            "data.frame must be (%s), got (%s)."),
                      length(provcolnames),
                      paste(provcolnames, collapse = ", "),
                      paste(names(provstore)[1:length(provcolnames)], collapse = ", ")
                      )
        stop(msg)
    }
    .dftoprovgraph(provstore)
})

.dftoprovgraph = function(df) {
    df2 = df[df$inputvar != "", ]
    edges = cbind(paste(df2$inputvar, df2$inputvarhash, sep=":"),
                  paste(df2$outputvar, df2$outputvarhash, sep=":"))
    gr = graph_from_edgelist(edges)
    orphans = provextranodes(df)
    if(length(orphans) > 0)
        gr = gr + vertices(orphans)
    gr
        
}

## this function ensures that variable values which never act as an
## input for another variable still appear correctly on the graph.
provextranodes = function(df) {
    allinp = paste(df$inputvars, df$inputvarhashes, sep = ":")
    allout = paste(df$outputvars, df$outputvarhashes, sep = ":")
    outvals = allout[df$inputvars == ""]
    invals = allinp[df$inputvars != ""]
    outvals = outvals[!(outvals %in% invals)]
    outvals

}

setGeneric("rbind", signature = "...", def = base::rbind)
##' @title rbind method
##' @param \dots Two or more ProvStoreDF objects. Must all have
##'     identical hashprefix values
##' @param deparse.level ignored.
##' @return A ProvStoreDF object.
##' @export
setMethod("rbind", "ProvStoreDF",
          function(..., deparse.level = 1) {
    args = list(...)
    cls = sapply(args, class)
    hpref = sapply(args, hashprefix)
    ## just for now
    stopifnot(identical(unique(cls), "ProvStoreDF"),
              length(unique(hpref)) == 1)

    dflist = lapply(args, provdata)
    df = do.call(rbind, dflist)
    ProvStoreDF(df = df, hashprefix = unique(hpref))
    
})

##' @title Combine provenance stores
##' @description This operation is conceptually 
##' @param \dots Two or more ProvStoreDF objects
##' @return A ProvStoreDF object
ProvStores = function(...) {
    args = list(...)
    cls = sapply(args, class)
    hpref = sappluy(args, hashprefix)
    ## just for now
    stopifnot(identical(unique(cls), "ProvStoreDF"),
              length(unique(hpref)) == 1)

    dflist = lapply(args, provdata)
    df = do.call(rbind, dflist)
    ProvStoreDF(df = df,  hashprefix = unique(hpref))
}   
