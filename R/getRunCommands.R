#'
#'@title Generate run commands for an ADMB model run
#'
#'@description Function to generate a script to make an ADMB model run
#'
#'@param os - 'win' or 'mac'
#'@param path2model -
#'@param pin -
#'@param hess -
#'@param mcmc -
#'
#'@return string with the run commands
#'
#'@export
#'
getRunCommands<-function(os='osx',
                         path2model=getPath2REM(),
                         pin=FALSE,
                         hess=TRUE,
                         mcmc=FALSE){
    #get the model name
    
    if (tolower(os)=='win'){
        model1<-basename(path2model);            #will include exe
        model<-substr(model1,1,length(model1)-4);#just the model name
        run.cmds<-'echo on
                    copy &&path2model &&model1
                    &&model -rs -nox  &&mcmc &&nohess &&jitter &&seed &&pin
                    del &&model1
                    del &&model.bar
                    del &&model.b0*
                    del &&model.p0*
                    del &&model.r0*
                    del variance
                    del fmin.log
                    echo off';
    } else if (tolower(os) %in% c('mac','osx')){
        model1<-NULL;
        model<-basename(path2model);
        run.cmds<-'#!/bin/sh
                  echo on
                  DIR="$( cd "$( dirname "$0" )" && pwd )"
                  cd ${DIR}
                  cp &&path2model ./&&model
                  ./&&model -rs -nox  &&mcmc &&nohess &&pin
                  rm &&model
                  rm &&model.bar
                  rm &&model.b0*
                  rm &&model.p0*
                  rm &&model.r0*
                  rm variance
                  rm fmin.log
                  echo off';
    }
    run.cmds<-gsub("&&path2model",  path2model,  run.cmds,fixed=TRUE);
    if (!is.null(model1)) run.cmds<-gsub("&&model1",model1,run.cmds,fixed=TRUE);
    if (!is.null(model))  run.cmds<-gsub("&&model", model, run.cmds,fixed=TRUE);
    str<-''; if (pin) str<-"-pin"
    run.cmds<-gsub("&&pin",str,run.cmds,fixed=TRUE)
    str<-''; if (!hess) str<-"-nohess"
    run.cmds<-gsub("&&nohess",str,run.cmds,fixed=TRUE)
    str<-''; if (mcmc) str<-"-mcmc"
    run.cmds<-gsub("&&mcmc",str,run.cmds,fixed=TRUE)
    return(run.cmds);
}