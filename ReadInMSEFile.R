##  CONFIGURE HERE
##
##  point "fullqualifiedFileName" to your mse file.
##
fullqualifiedFileName = "D:/xt/projects/SoftwareAnalyticsWithR/example_mse_files/junit.mse"

##
## Disclaimer - Currently this is more or less a hack, whose aim is to show
## that the pipeline inFamix parser, read in with R, visualize with R works
##
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

####################################################################
##  COntext class Creator Function
##  container for data as parsed from MSE File
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
        #print(sprintf("ATTRIBUTE name=%s; val=%s\n",name,as.character(val)));
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
#################################################################################
## Tokenizer Creator Function
##
newTokenizer <- function(fileName) {
    lahead <- NULL;
    currentLine <- NULL;
    currentLineNo <- 0;
    value <- NULL;
    currentPos <- -1;
    con <- file(fileName); 
    open(con,open = "r");
    eof = FALSE;
    #
    checkLineEmpty <- function() {
        if(is.null(currentLine) || nchar(currentLine) == 0) {
            currentLine <<- NULL;
            return (TRUE);
        }
        return (FALSE);
    } 
    #
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
    #
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
                    if("STR"==TOK) {
                        #strip away the ' around STR values
                        value <<- substr(currentLine,2,len-1);
                    } else {
                        value <<- substr(currentLine,1,len);
                    }
                    currentLine <<- substr(currentLine,len+1,nchar(currentLine));
                    lahead <<- TOK;
                    return (TOK);
                }
            }
        }
        stop(paste("lexer error:",currentLine,"; at line: ",currentLineNo))
    }
    #
    close <- function() {
        base:::close(con);
    }
    #
    isEof <- function() {
        eof;
    }
    #
    getValue <- function() {
        return (value);
    }
    #
    match <- function(token) {
        if(lahead == token) {
            lahead <<- getNextToken();
            #print(sprintf("%s -> %s; %s",lookahead, tok$getValue(), tok$isEof()));
        } else {
            stop(paste0("match error lookahead: ",lahead,"; expected: ",token))
        }
    }
    #
    getLA <- function() {
        lahead;
    }
    #
    isLA <- function(token) {
        return (lahead == token);
    }
    #
    list (
        getNextToken = getNextToken,
        getValue = getValue,
        isEof = isEof,
        close = close,
        match = match,
        lookahead = getLA,
        isLA = isLA
    );
    
}

tok <- newTokenizer(fullqualifiedFileName);

##
##

##

##################################################################################
## MSE Parser
##
newMSEParser <- function(tokenizer) {
    classData <- data.frame();
    pkgData <- data.frame();
    classLines <- 1;
    pkgLines <- 1;
    ##
    expr <- function(ctx) {
        while("OPEN"==tokenizer$lookahead()) {
            tokenizer$match("OPEN")
            if(tokenizer$isLA("IDENT")) {
                nam <- tokenizer$getValue();
                tokenizer$match("IDENT")
                val <- tokenizer$getValue();
                switch(
                    tokenizer$lookahead(),
                    OPEN  = { oCtx<-ctx; 
                              ctx<-newContext();
                              oCtx$addContext(ctx);
                              ctx$setCtxName(nam); 
                              expr(ctx); 
                              ctx<-oCtx; 
                              tokenizer$match("CLOSE");},
                    NUM   = { tokenizer$match("NUM");  ctx$addAttrib(nam,as.numeric(val)); tokenizer$match("CLOSE");   },
                    STR   = { tokenizer$match("STR");  ctx$addAttrib(nam,as.character(val)); tokenizer$match("CLOSE");},
                    BOOL  = { tokenizer$match("BOOL"); ctx$addAttrib(nam,as.logical(val)); tokenizer$match("CLOSE");  },
                    CLOSE = { tokenizer$match("CLOSE"); return}
                );
            } else if (tokenizer$isLA("ID")) {
                val <- identifier();
                ctx$setId(val);
                tokenizer$match("CLOSE");
            } else if (tokenizer$isLA("REF")) {
                val<-reference();
                ctx$setRefId(val);
                tokenizer$match("CLOSE");
            } else if (tokenizer$isLA("CLOSE")) {
                tokenizer$match("CLOSE");
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
    identifier <- function() {
        tokenizer$match("ID");
        val <- tokenizer$getValue();
        #print(sprintf("EMIT ID: %s\n",val));
        tokenizer$match("NUM");
        return (as.numeric(val));
    }
    ##
    reference <- function() {
        tokenizer$match("REF");
        val <- tokenizer$getValue();
        #print(sprintf("EMIT REF: %s\n",val));
        tokenizer$match("NUM");
        return (as.numeric(val));
    }
    ##
    root <- function() {
        ctx<-newContext();
        tokenizer$match("OPEN");
        expr(ctx);
        tokenizer$match("CLOSE");
    }
    ##
    parse <- function() {
        while(!tok$isEof()) {
            lookahead <- tok$getNextToken();
            if(tok$isEof()) {
                break;
            }
            print(sprintf("%s -> %s; %s",lookahead, tok$getValue(), tok$isEof()));
            root();
        }
        return (
            list(classData=classData,pkgData=pkgData)
            );
    }
    list(
        parse = parse
    );
}

mseParser <- newMSEParser(tok);
mseData <- mseParser$parse();
classData <- mseData[[1]];
pkgData <- mseData[[2]];
tok$close();
##
## about colnames, i.e. metric names
##
## http://habanero.ifi.uzh.ch/javaFamixMetrics/
## https://github.com/mircealungu/Softwarenaut/tree/master/dist-base/tools/inFusion
##
colNames <- c(
    "AMW",  #Average Method Weight (AMW) for every class. The average static complexity (in our case, McCabe's) of the methods of the measured class
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
##build a parent child relation in package data based on string prefixes
pkgData<-pkgData[order(nchar(pkgData$name),decreasing = TRUE),];
pkgData[,"parentPackage"] <- NA;
for(idx in 1:dim(pkgData)[1]) {
    aPkg <- pkgData$name[idx];
    largerOrEqualInSize <- nchar(pkgData$name) >= nchar(aPkg);
    hasNoParentSetYet <- is.na(pkgData[,"parentPackage"]);
    notSelf <- 1:dim(pkgData)[1] != idx
    combinedIndex <- which(largerOrEqualInSize & hasNoParentSetYet & notSelf);
    if(length(combinedIndex) > 1) {
        matches <- grep(paste0("^",aPkg,".*"),pkgData[combinedIndex,"name"])
        cat(matches);
        if(length(matches)!=0) {
            pkgData[combinedIndex[matches],"parentPackage"] <- pkgData[idx,"ID"];
        }
    }
}
##
##ADD a artificial root Node and point all packages which have still parent == "NA" to it. 
##A single root node is required by googleVis. 
rootRowIdx <- dim(pkgData)[1]+1;
rootID <- max( max(pkgData$ID), max(classData$ID) ) + 1;
pkgData[is.na(pkgData[,"parentPackage"]),"parentPackage"] <- rootID;
pkgData[rootRowIdx,"name"] <- "root";
pkgData[rootRowIdx,"parentPackage"] <- NA;
pkgData[rootRowIdx,"ID"] <- rootID;
##
##
##addedLines <- dim(classData)[1]+1:dim(pkgData)[1];
##classData[addedLines,"ID"]<-pkgData[,"ID"];
##classData[addedLines,"parentPackage"]<-pkgData[,"parentPackage"];
##classData[addedLines,"name"]<-pkgData[,"name"];
##
##
mergedData <- merge(classData,pkgData[,c("ID","name")], by.x="parentPackage", by.y="ID")
##
##
library("treemap")
par(mfrow=c(2,2))
treemap(dtf=mergedData,index=c("name.y","name.x"),"NOM")
plot(x=classData[,"RFC"],y=classData[,"LCOM"])
plot(x=classData[,"NOM"],y=classData[,"LCOM"])
boxplot(LCOM~RFC, data=classData)
boxplot(LCOM~NOM, data=classData)
boxplot(LCOM~AMW, data=classData)
##
##
##google vis cannot use a ID column...what the heck
library(googleVis)
classData$ident <- classData$ID
tmHtml <- gvisTreeMap(classData,
            idvar = "ident", parentvar = "parentPackage",
            sizevar = "ID", colorvar = "ID",
            options = list(),
            "STUFF");