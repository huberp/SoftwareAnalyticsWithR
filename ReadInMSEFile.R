##  CONFIGURE HERE
##
##  point "fullqualifiedFileName" to your mse file.
##
fullqualifiedFileName = "D:/xt/projects/SoftwareAnalyticsWithR/example_mse_files/junit.mse"

##
## Disclaimer - Currently this is more or less a hack, whose aim is to show
## that the pipeline inFamix parser, read in with R, visualize with R works
##

##http://rmod.lille.inria.fr/archives/reports/Duca11c-Cutter-deliverable22-MSE-FAMIX30.pdf
##2.2.1 Grammar
TOKENS <- list(
    OPEN  = "\\(",
    CLOSE = "\\)",
    REF   = "ref:",
    ID    = "id:",
    BOOL  = "true|false",
    IDENT = "[[:alpha:]]\\w*(\\.[[:alpha:]]*)*", #identifier even with dot notation
    NUM   = "[+-]?[[:digit:]]*(\\.[[:digit:]]*)?",    #number possibly with fraction 
    STR   = "'.*'",
    DOT   = "\\.",
    EOF   = -1,
    FAULT = -2
);
##
##
##
newContext <- function() {
    attributes <- list();
    contexts <- list();
    cID <- -1;
    refID <- -1;
    cName <- "";
    addContext <- function(aCtx) {
        contexts[[length(contexts)+1]] <<- eval(aCtx);
    }
    getContextByName <- function(aCtxName) {
        for(aCtx in contexts) {
            if(aCtxName==aCtx$getCtxName()) {
                return (aCtx);
            }
        }
        return (NULL);
    }
    addAttrib <- function(name,val) {
        print(sprintf("ATTRIBUTE name=%s; val=%s\n",name,as.character(val)));
        attributes[[name]] <<- val;
    }
    getAttribs <- function() {
        attributes;
    }
    setCtxName <- function(ctxName) {
        cName <<- ctxName;
    }
    getCtxName <- function() {
        cName;
    }
    setId <- function(id) {
        cID <<- id; 
    }
    getId <- function() {
        cID; 
    }
    setRefId <- function(refId) {
        refID <<- refId; 
    }
    getRefId <- function() {
        refID;
    }
    toString <- function() {
        res <- sprintf("Context: name='%s', ID=%d",cName,cID);
        for(n in names(attributes)) {
            res <- paste0(res,sprintf("\n%s -> %s",n, attributes[[n]]));
        }
        res <- paste0(res, sprintf("\n\tReference: %d",refID));
        if(0 < length(contexts)) {
            for(i in 1:length(contexts)) {
                co <- contexts[[i]];
                res <- paste0(res,sprintf("\n%s",co$toString()));
            }
        }
        res;
    }
    list (
        addContext = addContext,
        getContextByName = getContextByName,
        addAttrib = addAttrib,
        getAttribs = getAttribs, 
        setCtxName = setCtxName,
        getCtxName = getCtxName,
        setId = setId,
        getId = getId,
        setRefId = setRefId,
        getRefId = getRefId,
        toString = toString
    );
}
##
##
##
tokenizer <- function(fileName) {
    currentLine <- NULL;
    currentLineNo <- 0;
    value <- NULL;
    currentPos <- -1;
    
    con <- file(fileName); 
    open(con,open = "r");
    eof = FALSE;
    
    checkLineEmpty <- function() {
        if(is.null(currentLine) || nchar(currentLine) == 0) {
            currentLine <<- NULL;
            return (TRUE);
        }
        return (FALSE);
    } 
    
    loadNextLineOnDemand <- function() {
        ## load line (skipping empty lines)
        while(checkLineEmpty()) {
            currentLine <<- readLines(con, n=1);
            currentLineNo <<- currentLineNo + length(currentLine);   
            if(length(currentLine)!=1) {
                eof<<-TRUE;
                currentLine <<- NULL;
                return (0);
            }
        }
        return (1);
    }
    
    getNextToken <- function() {
        ## load line (skipping empty lines)
        while(TRUE) {
            loadNextLineOnDemand();
            if(isEof()) {
                return (TOKENS$EOF);
            }
            ## slurp away spaces
            match <- regexpr("[[:space:]]*",currentLine);
            len <- attr(match,"match.length");
            if(match[1]==1 && 0!=len) {
                currentLine <<- substr(currentLine,len+1,nchar(currentLine))
            }
            if(!checkLineEmpty()) {
                break;
            }
        }
        ##
        for(TOK in names(TOKENS)) {
            if(is.character(TOKENS[[TOK]])) {
                match <- regexpr(TOKENS[[TOK]],currentLine);
                len <- attr(match,"match.length");
                if(match[1]==1 && 0!=len) {
                    value <<- substr(currentLine,1,len);
                    currentLine <<- substr(currentLine,len+1,nchar(currentLine));
                    return (TOK);
                }
            }
        }
        stop(paste("lexer error:",currentLine,"; at line: ",currentLineNo))
    }
    
    close <- function() {
        base:::close(con);
    }
    
    isEof <- function() {
        eof;
    }
    
    getValue <- function() {
        return (value);
    }
    
    list (
        getNextToken = getNextToken,
        getValue = getValue,
        isEof = isEof,
        close = close
    );
    
}

lookahead <- NA;
tok <- tokenizer(fullqualifiedFileName);

match <- function(token) {
    if(lookahead == token) {
        lookahead <<- tok$getNextToken();
        #print(sprintf("%s -> %s; %s",lookahead, tok$getValue(), tok$isEof()));
    } else {
        stop(paste0("match error lookahead: ",lookahead,"; expected: ",token))
    }
}
##
##
root <- function() {
    ctx<-newContext();
    match("OPEN");
    expr(ctx);
    match("CLOSE");
}
##
classData <- data.frame();
pkgData <- data.frame();
classLines <- 1;
pkgLines <- 1;
##
expr <- function(ctx) {
    while("OPEN"==lookahead) {
        match("OPEN")
        if("IDENT"==lookahead) {
            nam <- tok$getValue();
            match("IDENT")
            val <- tok$getValue();
            switch(
                lookahead,
                OPEN  = { oCtx<-ctx; 
                          ctx<-newContext();
                          oCtx$addContext(ctx);
                          ctx$setCtxName(nam); 
                          expr(ctx); 
                          ctx<-oCtx; match("CLOSE");},
                NUM   = { match("NUM");  ctx$addAttrib(nam,as.numeric(val)); match("CLOSE");   },
                STR   = { match("STR");  ctx$addAttrib(nam,as.character(val)); match("CLOSE");},
                BOOL  = { match("BOOL"); ctx$addAttrib(nam,as.logical(val)); match("CLOSE");  },
                CLOSE = { match("CLOSE"); return}
            );
        } else if ("ID" == lookahead) {
            val <- identifier();
            ctx$setId(val);
            match("CLOSE");
        } else if ("REF" == lookahead) {
            val<-reference();
            ctx$setRefId(val);
            match("CLOSE");
        } else if ("CLOSE" == lookahead) {
            match("CLOSE");
            return;
        } 
    }
    if(!is.null(ctx) && ctx$getCtxName()=="FAMIX.Class") {
        cat(ctx$toString());
        cat("\n")
        attribs <- ctx$getAttribs();
        for(n in names(attribs)) {
            classData[classLines,n] <<- attribs[[n]];
        }
        classData[classLines,"ID"] <<- ctx$getId();
        parentPackageSubCtx <- ctx$getContextByName("parentPackage");
        if(!is.null(parentPackageSubCtx)) {
            classData[classLines,"parentPackage"] <<- parentPackageSubCtx$getRefId();
        }
        classLines <<- classLines+1;
    }
    if(!is.null(ctx) && ctx$getCtxName()=="FAMIX.Package") {
        attribs <- ctx$getAttribs();
        for(n in names(attribs)) {
            pkgData[pkgLines,n] <<- attribs[[n]];
        }
        pkgData[pkgLines,"ID"] <<- ctx$getId();
        pkgLines <<- pkgLines+1;
    }
}
##
##
identifier <- function() {
    match("ID");
    val <- tok$getValue();
    print(sprintf("EMIT ID: %s\n",val));
    match("NUM");
    return (as.numeric(val));
}

reference <- function() {
    match("REF");
    val <- tok$getValue();
    print(sprintf("EMIT REF: %s\n",val));
    match("NUM");
    return (as.numeric(val));
}

while(!tok$isEof()) {
    lookahead <- tok$getNextToken();
    if(tok$isEof()) {
        break;
    }
    print(sprintf("%s -> %s; %s",lookahead, tok$getValue(), tok$isEof()));
    root();
}
tok$close();
##
##
##
colNames <- c(
    "AMW",
    "BOvR",
    "BUR",
    "CPFD",
    "CW",
    "CBO",  #Coupling Between objects
    "DIT",
    "LCOM", #Lack of cohesion of methods
    "LCC",
    "NOAM",
    "NOACCM",
    "NAS",
    "NOA",
    "NOCHLD",
    "NOM",
    "NOVRM",
    "NOPRTA",
    "NOPRTM",
    "NOPUBA",
    "NOPUBM",
    "PNAS",
    "RFC",   #Response For Class
    "SPIDX",
    "TCC",
    "WOC"
)
##
colIndexes <- which(names(classData) %in% colNames)
##
##clean data for having only complete cases. it will sort out all abstract
##or interface types which have no metric values
classData <- classData[complete.cases(classData[,colIndexes]), ]
##
##
##
library("treemap")
treemap(dtf=classData,index=c("parentPackage","ID"),"LCOM")
plot(x=classData[,"RFC"],y=classData[,"LCOM"])
boxplot(LCOM~RFC, data=classData)