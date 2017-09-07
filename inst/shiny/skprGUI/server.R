library(skpr)
library(rintrojs)

shinyServer(
function(input, output, session) {

  incProgressSession = function(amount=0.1,message=NULL,detail=NULL) {incProgress(amount,message,detail,session)}

  inputlist_htc = reactive({
    input$submitbutton
    inputlist1 = list()
    for(i in 1:6) {
      if((i == 1 && (input$numberfactors) > 0) && isolate(input$blockdepth1) == "htc") {
        if((input$factortype1) == "numeric") {
          inputlist1[[(input$factorname1)]] = seq((input$numericlow1),(input$numerichigh1),length.out = input$numericlength1)
        }
        if((input$factortype1) == "discnum") {
          inputlist1[[(input$factorname1)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels1,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype1) == "cat") {
          inputlist1[[(input$factorname1)]] = strsplit(gsub(" ", "", input$levels1,fixed=TRUE),split=",")[[1]]
        }
      }
      if((i == 2 && (input$numberfactors) > 1) && isolate(input$blockdepth2) == "htc") {
        if((input$factortype2) == "numeric") {
          inputlist1[[(input$factorname2)]] = seq((input$numericlow2),(input$numerichigh2),length.out = input$numericlength2)
        }
        if((input$factortype2) == "discnum") {
          inputlist1[[(input$factorname2)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels2,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype2) == "cat") {
          inputlist1[[(input$factorname2)]] = strsplit(gsub(" ", "", input$levels2,fixed=TRUE),split=",")[[1]]
        }
      }
      if((i == 3 && (input$numberfactors) > 2) && isolate(input$blockdepth3) == "htc") {
        if((input$factortype3) == "numeric") {
          inputlist1[[(input$factorname3)]] = seq((input$numericlow3),(input$numerichigh3),length.out = input$numericlength3)
        }
        if((input$factortype3) == "discnum") {
          inputlist1[[(input$factorname3)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels3,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype3) == "cat") {
          inputlist1[[(input$factorname3)]] = strsplit(gsub(" ", "", input$levels3,fixed=TRUE),split=",")[[1]]
        }
      }
      if((i == 4 && (input$numberfactors) > 3) && isolate(input$blockdepth4) == "htc") {
        if((input$factortype4) == "numeric") {
          inputlist1[[(input$factorname4)]] = seq((input$numericlow4),(input$numerichigh4),length.out = input$numericlength4)
        }
        if((input$factortype4) == "discnum") {
          inputlist1[[(input$factorname4)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels4,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype4) == "cat") {
          inputlist1[[(input$factorname4)]] = strsplit(gsub(" ", "", input$levels4,fixed=TRUE),split=",")[[1]]
        }
      }
      if((i == 5 && (input$numberfactors) > 4) && isolate(input$blockdepth5) == "htc") {
        if((input$factortype5) == "numeric") {
          inputlist1[[(input$factorname5)]] = seq((input$numericlow5),(input$numerichigh5),length.out = input$numericlength5)
        }
        if((input$factortype5) == "discnum") {
          inputlist1[[(input$factorname5)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels5,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype5) == "cat") {
          inputlist1[[(input$factorname5)]] = strsplit(gsub(" ", "", input$levels5,fixed=TRUE),split=",")[[1]]
        }
      }
      if((i == 6 && (input$numberfactors) > 5) && isolate(input$blockdepth6) == "htc") {
        if((input$factortype6) == "numeric") {
          inputlist1[[(input$factorname6)]] = seq((input$numericlow6),(input$numerichigh6),length.out = input$numericlength6)
        }
        if((input$factortype6) == "discnum") {
          inputlist1[[(input$factorname6)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels6,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype6) == "cat") {
          inputlist1[[(input$factorname6)]] = strsplit(gsub(" ", "", input$levels6,fixed=TRUE),split=",")[[1]]
        }
      }
    }
    inputlist1
  })

  inputlist_htctext = reactive({
    inputlist1 = list()
    for(i in 1:6) {
      if((i == 1 && (input$numberfactors) > 0) && (input$blockdepth1) == "htc") {
        if((input$factortype1) == "numeric") {
          inputlist1[[(input$factorname1)]] = seq((input$numericlow1),(input$numerichigh1),length.out = input$numericlength1)
        }
        if((input$factortype1) == "discnum") {
          inputlist1[[(input$factorname1)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels1,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype1) == "cat") {
          inputlist1[[(input$factorname1)]] = strsplit(gsub(" ", "", input$levels1,fixed=TRUE),split=",")[[1]]
        }
      }
      if((i == 2 && (input$numberfactors) > 1) && (input$blockdepth2) == "htc") {
        if((input$factortype2) == "numeric") {
          inputlist1[[(input$factorname2)]] = seq((input$numericlow2),(input$numerichigh2),length.out = input$numericlength2)
        }
        if((input$factortype2) == "discnum") {
          inputlist1[[(input$factorname2)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels2,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype2) == "cat") {
          inputlist1[[(input$factorname2)]] = strsplit(gsub(" ", "", input$levels2,fixed=TRUE),split=",")[[1]]
        }
      }
      if((i == 3 && (input$numberfactors) > 2) && (input$blockdepth3) == "htc") {
        if((input$factortype3) == "numeric") {
          inputlist1[[(input$factorname3)]] = seq((input$numericlow3),(input$numerichigh3),length.out = input$numericlength3)
        }
        if((input$factortype3) == "discnum") {
          inputlist1[[(input$factorname3)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels3,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype3) == "cat") {
          inputlist1[[(input$factorname3)]] = strsplit(gsub(" ", "", input$levels3,fixed=TRUE),split=",")[[1]]
        }
      }
      if((i == 4 && (input$numberfactors) > 3) && (input$blockdepth4) == "htc") {
        if((input$factortype4) == "numeric") {
          inputlist1[[(input$factorname4)]] = seq((input$numericlow4),(input$numerichigh4),length.out = input$numericlength4)
        }
        if((input$factortype4) == "discnum") {
          inputlist1[[(input$factorname4)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels4,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype4) == "cat") {
          inputlist1[[(input$factorname4)]] = strsplit(gsub(" ", "", input$levels4,fixed=TRUE),split=",")[[1]]
        }
      }
      if((i == 5 && (input$numberfactors) > 4) && (input$blockdepth5) == "htc") {
        if((input$factortype5) == "numeric") {
          inputlist1[[(input$factorname5)]] = seq((input$numericlow5),(input$numerichigh5),length.out = input$numericlength5)
        }
        if((input$factortype5) == "discnum") {
          inputlist1[[(input$factorname5)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels5,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype5) == "cat") {
          inputlist1[[(input$factorname5)]] = strsplit(gsub(" ", "", input$levels5,fixed=TRUE),split=",")[[1]]
        }
      }
      if((i == 6 && (input$numberfactors) > 5) && (input$blockdepth6) == "htc") {
        if((input$factortype6) == "numeric") {
          inputlist1[[(input$factorname6)]] = seq((input$numericlow6),(input$numerichigh6),length.out = input$numericlength6)
        }
        if((input$factortype6) == "discnum") {
          inputlist1[[(input$factorname6)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels6,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype6) == "cat") {
          inputlist1[[(input$factorname6)]] = strsplit(gsub(" ", "", input$levels6,fixed=TRUE),split=",")[[1]]
        }
      }
    }
    inputlist1
  })


  inputlist = reactive({
    inputlist1 = list()
    for(i in 1:6) {
      if(i == 1 && (input$numberfactors) > 0 ) {
        if(input$blockdepth1 == "etc") {
          if((input$factortype1) == "numeric") {
            inputlist1[[(input$factorname1)]] = seq((input$numericlow1),(input$numerichigh1),length.out = input$numericlength1)
          }
          if((input$factortype1) == "discnum") {
            inputlist1[[(input$factorname1)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels1,fixed=TRUE),split=",")[[1]])
          }
          if((input$factortype1) == "cat") {
            inputlist1[[(input$factorname1)]] = strsplit(gsub(" ", "", input$levels1,fixed=TRUE),split=",")[[1]]
          }
        }
      }
      if(i == 2 && (input$numberfactors) > 1) {
        if(input$blockdepth2 == "etc") {
          if((input$factortype2) == "numeric") {
            inputlist1[[(input$factorname2)]] = seq((input$numericlow2),(input$numerichigh2),length.out = input$numericlength2)
          }
          if((input$factortype2) == "discnum") {
            inputlist1[[(input$factorname2)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels2,fixed=TRUE),split=",")[[1]])
          }
          if((input$factortype2) == "cat") {
            inputlist1[[(input$factorname2)]] = strsplit(gsub(" ", "", input$levels2,fixed=TRUE),split=",")[[1]]
          }
        }
      }
      if(i == 3 && (input$numberfactors) > 2) {
        if(input$blockdepth3 == "etc") {
          if((input$factortype3) == "numeric") {
            inputlist1[[(input$factorname3)]] = seq((input$numericlow3),(input$numerichigh3),length.out = input$numericlength3)
          }
          if((input$factortype3) == "discnum") {
            inputlist1[[(input$factorname3)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels3,fixed=TRUE),split=",")[[1]])
          }
          if((input$factortype3) == "cat") {
            inputlist1[[(input$factorname3)]] = strsplit(gsub(" ", "", input$levels3,fixed=TRUE),split=",")[[1]]
          }
        }
      }
      if(i == 4 && (input$numberfactors) > 3) {
        if(input$blockdepth4 == "etc") {
          if((input$factortype4) == "numeric") {
            inputlist1[[(input$factorname4)]] = seq((input$numericlow4),(input$numerichigh4),length.out = input$numericlength4)
          }
          if((input$factortype4) == "discnum") {
            inputlist1[[(input$factorname4)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels4,fixed=TRUE),split=",")[[1]])
          }
          if((input$factortype4) == "cat") {
            inputlist1[[(input$factorname4)]] = strsplit(gsub(" ", "", input$levels4,fixed=TRUE),split=",")[[1]]
          }
        }
      }
      if(i == 5 && (input$numberfactors) > 4) {
        if(input$blockdepth5 == "etc") {
          if((input$factortype5) == "numeric") {
            inputlist1[[(input$factorname5)]] = seq((input$numericlow5),(input$numerichigh5),length.out = input$numericlength5)
          }
          if((input$factortype5) == "discnum") {
            inputlist1[[(input$factorname5)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels5,fixed=TRUE),split=",")[[1]])
          }
          if((input$factortype5) == "cat") {
            inputlist1[[(input$factorname5)]] = strsplit(gsub(" ", "", input$levels5,fixed=TRUE),split=",")[[1]]
          }
        }
      }
      if(i == 6 && (input$numberfactors) > 5) {
        if(input$blockdepth6 == "etc") {
          if((input$factortype6) == "numeric") {
            inputlist1[[(input$factorname6)]] = seq((input$numericlow6),(input$numerichigh6),length.out = input$numericlength6)
          }
          if((input$factortype6) == "discnum") {
            inputlist1[[(input$factorname6)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels6,fixed=TRUE),split=",")[[1]])
          }
          if((input$factortype6) == "cat") {
            inputlist1[[(input$factorname6)]] = strsplit(gsub(" ", "", input$levels6,fixed=TRUE),split=",")[[1]]
          }
        }
      }
    }
    inputlist1
  })

  candidatesetall = reactive({
    candidateset1 = list()
    for(i in 1:6) {
      if(i == 1 && (input$numberfactors) > 0 ) {
        if((input$factortype1) == "numeric") {
          candidateset1[[(input$factorname1)]] = seq((input$numericlow1),(input$numerichigh1),length.out = input$numericlength1)
        }
        if((input$factortype1) == "discnum") {
          candidateset1[[(input$factorname1)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels1,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype1) == "cat") {
          candidateset1[[(input$factorname1)]] = strsplit(gsub(" ", "", input$levels1,fixed=TRUE),split=",")[[1]]
        }
      }
      if(i == 2 && (input$numberfactors) > 1) {
        if((input$factortype2) == "numeric") {
          candidateset1[[(input$factorname2)]] = seq((input$numericlow2),(input$numerichigh2),length.out = input$numericlength2)
        }
        if((input$factortype2) == "discnum") {
          candidateset1[[(input$factorname2)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels2,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype2) == "cat") {
          candidateset1[[(input$factorname2)]] = strsplit(gsub(" ", "", input$levels2,fixed=TRUE),split=",")[[1]]
        }
      }
      if(i == 3 && (input$numberfactors) > 2) {
        if((input$factortype3) == "numeric") {
          candidateset1[[(input$factorname3)]] = seq((input$numericlow3),(input$numerichigh3),length.out = input$numericlength3)
        }
        if((input$factortype3) == "discnum") {
          candidateset1[[(input$factorname3)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels3,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype3) == "cat") {
          candidateset1[[(input$factorname3)]] = strsplit(gsub(" ", "", input$levels3,fixed=TRUE),split=",")[[1]]
        }
      }
      if(i == 4 && (input$numberfactors) > 3) {
        if((input$factortype4) == "numeric") {
          candidateset1[[(input$factorname4)]] = seq((input$numericlow4),(input$numerichigh4),length.out = input$numericlength4)
        }
        if((input$factortype4) == "discnum") {
          candidateset1[[(input$factorname4)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels4,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype4) == "cat") {
          candidateset1[[(input$factorname4)]] = strsplit(gsub(" ", "", input$levels4,fixed=TRUE),split=",")[[1]]
        }
      }
      if(i == 5 && (input$numberfactors) > 4) {
        if((input$factortype5) == "numeric") {
          candidateset1[[(input$factorname5)]] = seq((input$numericlow5),(input$numerichigh5),length.out = input$numericlength5)
        }
        if((input$factortype5) == "discnum") {
          candidateset1[[(input$factorname5)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels5,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype5) == "cat") {
          candidateset1[[(input$factorname5)]] = strsplit(gsub(" ", "", input$levels5,fixed=TRUE),split=",")[[1]]
        }
      }
      if(i == 6 && (input$numberfactors) > 5) {
        if((input$factortype6) == "numeric") {
          candidateset1[[(input$factorname6)]] = seq((input$numericlow6),(input$numerichigh6),length.out = input$numericlength6)
        }
        if((input$factortype6) == "discnum") {
          candidateset1[[(input$factorname6)]] = as.numeric(strsplit(gsub(" ", "", input$disclevels6,fixed=TRUE),split=",")[[1]])
        }
        if((input$factortype6) == "cat") {
          candidateset1[[(input$factorname6)]] = strsplit(gsub(" ", "", input$levels6,fixed=TRUE),split=",")[[1]]
        }
      }
    }
    candidateset1
  })



  inputstring = reactive({
    updatevector = c(input$blockdepth1,input$blockdepth2,input$blockdepth3,input$blockdepth4,input$blockdepth5,input$blockdepth6)
    commacount = input$numberfactors-1
    finalstring = c()
    for(i in 1:6) {
      if(i == 1 && (input$numberfactors) > 0) {
        finalstring = c(finalstring,(input$factorname1)," = ")
        if((input$factortype1) == "numeric") {
          finalstring = c(finalstring, "seq(",(input$numericlow1),",",(input$numerichigh1),", length.out=",input$numericlength1,")")
        }
        if((input$factortype1) == "discnum") {
          finalstring = c(finalstring, "c(",gsub(" ", "", input$disclevels1,fixed=TRUE),")")
        }
        if((input$factortype1) == "cat") {
          len = length(strsplit(input$levels1,split=",")[[1]])
          levelstring = paste0(c("\""),strsplit(gsub(" ", "", input$levels1,fixed=TRUE),split=",")[[1]],c("\","),collapse="")
          levelstring = substr(levelstring, 1, nchar(levelstring)-1)
          finalstring = c(finalstring, "c(",levelstring,")")
        }
        if(commacount > 0) {
          commacount = commacount - 1
          finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",27)),collapse=""))
        }
      }
      if((i == 2 && (input$numberfactors) > 1)) {
        finalstring = c(finalstring, input$factorname2," = ")
        if((input$factortype2) == "numeric") {
          finalstring = c(finalstring, "seq(",(input$numericlow2),",",(input$numerichigh2),", length.out=",input$numericlength2,")")
        }
        if((input$factortype2) == "discnum") {
          finalstring = c(finalstring, "c(",gsub(" ", "", input$disclevels2,fixed=TRUE),")")
        }
        if((input$factortype2) == "cat") {
          len = length(strsplit(input$levels2,split=",")[[1]])
          levelstring = paste0(c("\""),strsplit(gsub(" ", "", input$levels2,fixed=TRUE),split=",")[[1]],c("\","),collapse="")
          levelstring = substr(levelstring, 1, nchar(levelstring)-1)
          finalstring = c(finalstring, "c(",levelstring,")")
        }
        if(commacount > 0) {
          commacount = commacount - 1
          finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",27)),collapse=""))
        }
      }
      if(i == 3 && (input$numberfactors) > 2) {
        finalstring = c(finalstring,input$factorname3," = ")
        if((input$factortype3) == "numeric") {
          finalstring = c(finalstring, "seq(",(input$numericlow3),",",(input$numerichigh3),", length.out=",input$numericlength3,")")
        }
        if((input$factortype3) == "discnum") {
          finalstring = c(finalstring, "c(",gsub(" ", "", input$disclevels3,fixed=TRUE),")")
        }
        if((input$factortype3) == "cat") {
          len = length(strsplit(input$levels3,split=",")[[1]])
          levelstring = paste0(c("\""),strsplit(gsub(" ", "", input$levels3,fixed=TRUE),split=",")[[1]],c("\","),collapse="")
          levelstring = substr(levelstring, 1, nchar(levelstring)-1)
          finalstring = c(finalstring, "c(",levelstring,")")
        }
        if(commacount > 0) {
          commacount = commacount - 1
          finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",27)),collapse=""))
        }
      }
      if(i == 4 && (input$numberfactors) > 3) {
        finalstring = c(finalstring,input$factorname4," = ")
        if((input$factortype4) == "numeric") {
          finalstring = c(finalstring, "seq(",(input$numericlow4),",",(input$numerichigh4),", length.out=",input$numericlength4,")")
        }
        if((input$factortype4) == "discnum") {
          finalstring = c(finalstring, "c(",gsub(" ", "", input$disclevels4,fixed=TRUE),")")
        }
        if((input$factortype4) == "cat") {
          len = length(strsplit(input$levels4,split=",")[[1]])
          levelstring = paste0(c("\""),strsplit(gsub(" ", "", input$levels4,fixed=TRUE),split=",")[[1]],c("\","),collapse="")
          levelstring = substr(levelstring, 1, nchar(levelstring)-1)
          finalstring = c(finalstring, "c(",levelstring,")")
        }
        if(commacount > 0) {
          commacount = commacount - 1
          finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",27)),collapse=""))
        }
      }
      if(i == 5 && (input$numberfactors) > 4) {
        finalstring = c(finalstring,input$factorname5," = ")
        if((input$factortype5) == "numeric") {
          finalstring = c(finalstring, "seq(",(input$numericlow5),",",(input$numerichigh5),", length.out=",input$numericlength5,")")
        }
        if((input$factortype5) == "discnum") {
          finalstring = c(finalstring, "c(",gsub(" ", "", input$disclevels5,fixed=TRUE),")")
        }
        if((input$factortype5) == "cat") {
          len = length(strsplit(input$levels5,split=",")[[1]])
          levelstring = paste0(c("\""),strsplit(gsub(" ", "", input$levels5,fixed=TRUE),split=",")[[1]],c("\","),collapse="")
          levelstring = substr(levelstring, 1, nchar(levelstring)-1)
          finalstring = c(finalstring, "c(",levelstring,")")
        }
        if(commacount > 0) {
          commacount = commacount - 1
          finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",27)),collapse=""))
        }
      }
      if(i == 6 && (input$numberfactors) > 5) {
        finalstring = c(finalstring,input$factorname6," = ")
        if((input$factortype6) == "numeric") {
          finalstring = c(finalstring, "seq(",(input$numericlow6),",",(input$numerichigh6),", length.out=",input$numericlength6,")")
        }
        if((input$factortype6) == "discnum") {
          finalstring = c(finalstring, "c(",gsub(" ", "", input$disclevels6,fixed=TRUE),")")
        }
        if((input$factortype6) == "cat") {
          len = length(strsplit(input$levels6,split=",")[[1]])
          levelstring = paste0(c("\""),strsplit(gsub(" ", "", input$levels6,fixed=TRUE),split=",")[[1]],c("\","),collapse="")
          levelstring = substr(levelstring, 1, nchar(levelstring)-1)
          finalstring = c(finalstring, "c(",levelstring,")")
        }
      }
    }
    finalstring
  })

  regularmodelstring = reactive({
    if(input$model == "~.") {
      paste0("~ ",paste(c(names(inputlist())),collapse=" + "))
    } else {
      as.character(as.formula(input$model))
    }
  })

  modelwithblocks = reactive({
    if(isblockingtext()) {
      if(input$model == "~.") {
        inputterms = c(input$factorname1,input$factorname2,input$factorname3,input$factorname4,input$factorname5,input$factorname6)[1:input$numberfactors]
        basemodel = paste0("~",paste(inputterms,collapse=" + "))
      } else {
        basemodel = input$model
      }
      basemodel = gsub(pattern="~",replacement = "",x=basemodel,fixed=TRUE)
      blockingmodelterms = "~ (1|Block1) + "
      paste0(blockingmodelterms,basemodel)
    }
  })

  contraststring = reactive({
    factorcat = c(input$factortype1,input$factortype2,input$factortype3,input$factortype4,input$factortype5,input$factortype6) == "cat"
    namecat = c(input$factorname1,input$factorname2,input$factorname3,input$factorname4,input$factorname5,input$factorname6)[factorcat]
    contrasttemp = "list("
    for(i in 1:length(namecat)) {
      if(i != length(namecat)) {
        contrasttemp = paste0(contrasttemp, namecat[i] , " = ","contr.sum, ")
      } else {
        contrasttemp = paste0(contrasttemp, namecat[i] , " = ","contr.sum)")
      }
    }
    contrasttemp
  })

  anyfactors = reactive({
    any(c(input$factortype1,input$factortype2,input$factortype3,input$factortype4,input$factortype5,input$factortype6) == "cat")
  })

  code = reactive({
    blocking = any("htc" %in% c(input$blockdepth1,input$blockdepth2,input$blockdepth3,input$blockdepth4,input$blockdepth5,input$blockdepth6)[1:input$numberfactors])

    first = paste0(c("<br><pre>",
                     "<code style=\"color:#468449\"># This is the R code used to generate these results in skpr.</code><br>",
                     "<code style=\"color:#468449\"># Copy this into an R script and rerun to reproduce these results.</code><br><br>",
                     "library(skpr)<br><br>",
                     ifelse(input$setseed,
                            paste0("<code style=\"color:#468449\">#Setting random number generator seed:</code><br>",
                                   "set.seed(", input$seed, ")<br><br>"),""),
                     "<code style=\"color:#468449\"># Generating candidate set:</code><br>",
                     "candidateset = expand.grid(", inputstring(), ")<br><br>",
                     ifelse(blocking,
                            paste0(c("<code style=\"color:#468449\"># Generating design for hard-to-change factors:</code> <br>",
                                     "design_htc = gen_design(candidateset = candidateset, <br>", rep("&nbsp;",24),
                                     "model = ", blockmodel(), ",<br>", rep("&nbsp;",24),
                                     "trials = ", as.character(input$numberblocks),")<br><br>"),collapse=""),""),
                     "<code style=\"color:#468449\"># Generating design:</code><br>",
                     "design = gen_design(candidateset = candidateset, <br>", rep("&nbsp;",20),
                     "model = ", as.character(as.formula(input$model)), ",<br>", rep("&nbsp;",20),
                     "trials = ", as.character(input$trials)
    ),collapse="")
    if(blocking) {
      first = paste(c(first, ",<br>", rep("&nbsp;",20),
                      "splitplotdesign = design_htc"),collapse = "")
    }
    if(input$trials %% input$numberblocks == 0) {
      sizevector = input$trials/input$numberblocks
    } else {
      sizevector = c(rep(ceiling(input$trials/input$numberblocks),input$numberblocks))
      unbalancedruns = ceiling(input$trials/input$numberblocks)*input$numberblocks - input$trials
      sizevector[(length(sizevector)-unbalancedruns+1):length(sizevector)] = sizevector[(length(sizevector)-unbalancedruns+1):length(sizevector)] -1
      sizevector = paste0(c("c(",paste0(sizevector,collapse=","),")"),collapse="")
    }
    if(isblockingtext()) {
      first = paste(c(first, ",<br>", rep("&nbsp;",20),
                      "splitplotsizes = ",sizevector),collapse = "")
    }
    if(input$optimality != "D") {
      first = paste(c(first, ",<br>", rep("&nbsp;",20),
                      "optimality = \"",input$optimality,"\""),collapse = "")
    }
    if(input$repeats != 10) {
      first = paste(c(first, ",<br>", rep("&nbsp;",20),
                      "repeats = ",input$repeats),collapse = "")
    }
    if(input$varianceratio != 1) {
      first = paste(c(first, ",<br>", rep("&nbsp;",20),
                      "varianceratio = ",input$varianceratio),collapse = "")
    }
    if(input$aliaspower != 2) {
      first = paste(c(first, ",<br>", rep("&nbsp;",20),
                      "aliaspower = ",input$aliaspower),collapse = "")
    }
    if(input$mindopt != 0.95) {
      first = paste(c(first, ",<br>", rep("&nbsp;",20),
                      "minDopt = ",input$mindopt),collapse = "")
    }
    if(as.logical(input$parallel)) {
      first = paste(c(first, ",<br>", rep("&nbsp;",20),
                      "parallel = TRUE"),collapse = "")
    }
    if(isblockingtext()) {
      first = paste(c(first, ",<br>", rep("&nbsp;",20),
                      "splitcolumns = ", ifelse(input$splitanalyzable, "TRUE","FALSE")),collapse = "")
    }
    first = paste0(c(first,")<br><br>"),collapse="")
    if(input$evaltype == "lm") {
      first = paste0(c(first,
                       "<code style=\"color:#468449\"># Evaluating Design:</code><br>",
                       "eval_design(RunMatrix = design,<br>", rep("&nbsp;",12),
                       "model = ", as.character(as.formula(input$model)), ",<br>", rep("&nbsp;",12),
                       "alpha = ", input$alpha),collapse="")
      if(isblockingtext()) {
        first = paste(c(first, ",<br>", rep("&nbsp;",12),
                        "blocking = TRUE"),collapse = "")
      }
      if(input$snr != 2) {
        first = paste(c(first, ",<br>", rep("&nbsp;",12),
                        "effectsize = ",input$snr),collapse = "")
      }
      if(input$varianceratio != 1) {
        first = paste(c(first, ",<br>", rep("&nbsp;",12),
                        "varianceratios = ",input$varianceratio),collapse = "")
      }
      if(as.logical(input$conservative)) {
        first = paste(c(first, ",<br>", rep("&nbsp;",12),
                        "conservative = TRUE"),collapse = "")
      }
      if(as.logical(input$detailedoutput)) {
        first = paste(c(first, ",<br>", rep("&nbsp;",12),
                        "detailedoutput = TRUE"),collapse = "")
      }
      first = paste0(c(first,")<br><br>"),collapse="")
      first = paste0(first,
                     "<code style=\"color:#468449\">## How to analyze this experiment when the data have been collected:</code><br>",
                     "<code style=\"color:#468449\">## (to run, remove one # from this section) </code><br>",
                     "<code style=\"color:#468449\">## First, assign the results to a column in the data frame. Each </code><br>",
                     "<code style=\"color:#468449\">## entry in the vector corresponds to the result from that run in the design. <br><br></code>",
                     "<code style=\"color:#468449\">#design$Y = results <br><br></code>",
                     ifelse(!isblockingtext(),
                            "<code style=\"color:#468449\">## Now analyze the linear model with lm:</code><br><br>",
                            "<code style=\"color:#468449\">## Now analyze the blocked linear model with lmer (from the lme4 package):<br><br></code>"),
                     ifelse(!isblockingtext(),
                            paste0("<code style=\"color:#468449\">#lm(formula = Y ", regularmodelstring(),
                                   ", data = design" ,
                                   ifelse(anyfactors(),
                                          paste0(", </code><br><code style=\"color:#468449\">#   ", "contrasts = ",contraststring(),")</code>"),
                                          ")<br><br></code>")),
                            paste0(ifelse(input$splitanalyzable,"","<code style=\"color:#468449\">## Note: Argument splitcolumns needs to be active in last gen_design call in order<br>## to analyze data taking into account the split-plot structure. The code below assumes that is true. <br><br></code>"),
                                   "<code style=\"color:#468449\">#lme4::lmer(formula = Y ",
                                   modelwithblocks(),
                                   ", data = design",
                                   ifelse(anyfactors(),paste0(",<br>#          ","contrasts = ",contraststring(),"))<br><br>"),"))<br><br></code>"))))
    }
    if(input$evaltype == "glm") {
      first = paste0(c(first,
                       "<code style=\"color:#468449\"># Evaluating (Monte Carlo) Design:</code><br>",
                       "eval_design_mc(RunMatrix = design,<br>", rep("&nbsp;",15),
                       "model = ", as.character(as.formula(input$model)), ",<br>", rep("&nbsp;",15),
                       "alpha = ", input$alpha),collapse="")
      if(isblockingtext()) {
        first = paste(c(first, ",<br>", rep("&nbsp;",15),
                        "blocking = TRUE"),collapse = "")
      }
      if(input$nsim != 1000) {
        first = paste(c(first, ",<br>", rep("&nbsp;",15),
                        "nsim = ", input$nsim),collapse = "")
      }
      if(input$glmfamily != "gaussian") {
        first = paste(c(first, ",<br>", rep("&nbsp;",15),
                        "glmfamily = \"",input$glmfamily,"\""),collapse = "")
      }
      if(length(effectsize()) == 1) {
        first = paste(c(first, ",<br>", rep("&nbsp;",15),
                        "effectsize = ",effectsize()),collapse = "")
      } else {
        first = paste(c(first, ",<br>", rep("&nbsp;",15),
                        "effectsize = c(",effectsize()[1],", ", effectsize()[2],")"),collapse = "")
      }
      if(!is.null(input$varianceratios)) {
        first = paste(c(first, ",<br>", rep("&nbsp;",15),
                        "varianceratios = ",input$varianceratio),collapse = "")
      }
      if(as.logical(input$parallel_eval_glm)) {
        first = paste(c(first, ",<br>", rep("&nbsp;",15),
                        "parallel = TRUE"),collapse = "")
      }
      if(as.logical(input$detailedoutput)) {
        first = paste(c(first, ",<br>", rep("&nbsp;",15),
                        "detailedoutput = TRUE"),collapse = "")
      }
      first = paste0(c(first,")<br><br>"),collapse="")
      first = paste0(first,
                     "<code style=\"color:#468449\">## How to analyze this experiment when the data have been collected:</code><br>",
                     "<code style=\"color:#468449\">## (to run, remove one # from this section) </code><br>",
                     "<code style=\"color:#468449\">## First, assign the results to a column in the data frame. Each </code><br>",
                     "<code style=\"color:#468449\">## entry in the vector corresponds to the result from that run in the design. <br><br></code>",
                     "<code style=\"color:#468449\">#design$Y = results <br><br></code>",
                     ifelse(!isblockingtext(),
                            "<code style=\"color:#468449\">## Now analyze the generalized linear model with glm:</code><br><br>",
                            "<code style=\"color:#468449\">## Now analyze the blocked generalized linear model with glmer (from the lme4 package):<br><br></code>"),
                     ifelse(!isblockingtext(),
                            paste0("<code style=\"color:#468449\">#glm(formula = Y ", regularmodelstring(),
                                   ", data = design" ,
                                   ",<br>#   family = ", ifelse(input$glmfamily == "exponential", "Gamma(link=\"log\")",paste0("\"",input$glmfamily,"\"")),
                                   ifelse(anyfactors(),
                                          paste0(", </code><br><code style=\"color:#468449\">#   ", "contrasts = ",contraststring(),")</code>"),
                                          ")<br><br></code>")),
                            paste0(ifelse(input$splitanalyzable,"","<code style=\"color:#468449\">## Note: Argument splitcolumns needs to be active in last gen_design call in order<br>## to analyze data taking into account the split-plot structure. The code below assumes that is true. <br><br></code>"),
                                   "<code style=\"color:#468449\">#lme4::glmer(formula = Y ",
                                   modelwithblocks(),
                                   ", data = design",
                                   ",<br>#          family = ", ifelse(input$glmfamily == "exponential", "Gamma(link=\"log\")",paste0("\"",input$glmfamily,"\"")),
                                   ifelse(anyfactors(),paste0(",<br>#          ","contrasts = ",contraststring(),")"),")</code>"))))
    }
    if(input$evaltype == "surv") {
      first = paste0(c(first,
                       "<code style=\"color:#468449\"># Evaluating (Monte Carlo Survival) Design:</code><br>",
                       "eval_design_survival_mc(RunMatrix = design,<br>", rep("&nbsp;",24),
                       "model = ", as.character(as.formula(input$model)), ",<br>", rep("&nbsp;",24),
                       "alpha = ", input$alpha),collapse="")
      if(input$nsim_surv != 1000) {
        first = paste(c(first, ",<br>", rep("&nbsp;",24),
                        "nsim = ", input$nsim_surv),collapse = "")
      }
      if(input$distribution != "gaussian") {
        first = paste(c(first, ",<br>", rep("&nbsp;",24),
                        "distribution = \"",input$distribution,"\""),collapse = "")
      }
      if(length(effectsize()) == 1) {
        first = paste(c(first, ",<br>", rep("&nbsp;",24),
                        "effectsize = ",effectsize()),collapse = "")
      } else {
        first = paste(c(first, ",<br>", rep("&nbsp;",24),
                        "effectsize = c(",effectsize()[1],", ", effectsize()[2],")"),collapse = "")
      }
      if(!is.na(input$censorpoint)) {
        first = paste(c(first, ",<br>", rep("&nbsp;",24),
                        "censorpoint = ",input$censorpoint),collapse = "")
      }
      if(input$censortype != "right") {
        first = paste(c(first, ",<br>", rep("&nbsp;",24),
                        "censortype = \"",input$censortype,"\""),collapse = "")
      }
      if(as.logical(input$parallel_eval_surv)) {
        first = paste(c(first, ",<br>", rep("&nbsp;",24),
                        "parallel = TRUE"),collapse = "")
      }
      if(as.logical(input$detailedoutput)) {
        first = paste(c(first, ",<br>", rep("&nbsp;",24),
                        "detailedoutput = TRUE"),collapse = "")
      }
      first = paste0(c(first,")<br><br>"),collapse="")
      first = paste0(first,
                     "<code style=\"color:#468449\">## How to analyze this experiment when the data have been collected:</code><br>",
                     "<code style=\"color:#468449\">## (to run, remove one # from this section) </code><br>",
                     "<code style=\"color:#468449\">## This is a survival model, so first create a Surv object that marks the censored runs.</code><br>",
                     "<code style=\"color:#468449\">## Each entry in the results vector corresponds to the result from that run in the design.</code><br>",
                     "<code style=\"color:#468449\">## Here, the raw censored responses are given as NA. We replace those values with the</code><br>",
                     "<code style=\"color:#468449\">## censor point and use the event argument to mark them as censored. </code><br>",
                     "<code style=\"color:#468449\">## (Surv argument \"event\" is TRUE when \"results\" is not censored, and FALSE when censored).<br><br></code>",
                     "<code style=\"color:#468449\">#notcensored = !is.na(results) </code><br>",
                     ifelse(!is.na(input$censorpoint),paste0("<code style=\"color:#468449\">#results[is.na(results)] = ", input$censorpoint, "</code><br>"),
                            ""),
                     "<code style=\"color:#468449\">#design$Y = Surv(time=results, event = notcensored, type = \"", input$censortype,"\") <br><br></code>",
                     "<code style=\"color:#468449\">## Now analyze the linear model with survreg (from the survival package):</code><br><br>",
                     paste0("<code style=\"color:#468449\">#survival::survreg(formula = Y ", regularmodelstring(),
                            ", data = design, dist = \"", input$distribution,"\")"))
    }

    first = paste0(c(first,"</pre></code>"),collapse="")
    first
  })

  isblocking = reactive({
    input$submitbutton
    any("htc" %in% c(isolate(input$blockdepth1),
                     isolate(input$blockdepth2),
                     isolate(input$blockdepth3),
                     isolate(input$blockdepth4),
                     isolate(input$blockdepth5),
                     isolate(input$blockdepth6))[1:isolate(input$numberfactors)])
  })
  isblockingtext = reactive({
    any("htc" %in% c((input$blockdepth1),
                     (input$blockdepth2),
                     (input$blockdepth3),
                     (input$blockdepth4),
                     (input$blockdepth5),
                     (input$blockdepth6))[1:(input$numberfactors)])
  })

  blockmodel = reactive({
    if(input$model == "~.") {
      as.formula(paste0("~",paste(names(inputlist_htctext()),collapse=" + ")))
    } else {
      names = names(inputlist())
      modelsplit = attr(terms.formula(as.formula(input$model)), "term.labels")
      regularmodel = rep(FALSE, length(modelsplit))
      for(term in names) {
        regex = paste0("(\\b",term,"\\b)|(\\b",term,":)|(:",term,"\\b)|(\\b",term,"\\s\\*)|(\\*\\s",term,"\\b)|(:",term,":)")
        regularmodel = regularmodel | grepl(regex,modelsplit,perl=TRUE)
      }
      paste0("~",paste(modelsplit[!regularmodel],collapse=" + "))

    }
  })

  optimality = reactive({
    input$submitbutton
    if(isolate(input$numberfactors) == 1 && isolate(input$optimality) == "Alias") {
      showNotification("Alias-optimal design selected with only one factor: Switching to D-optimal.", type="warning",duration=10)
      updateSelectInput(session,"optimality", choices = c("D","I","A","Alias","G","E","T"), selected="D")
      "D"
    } else {
      isolate(input$optimality)
    }
  })

  effectsize = reactive({
    if(input$evaltype == "lm") {
      return(input$snr)
    }
    if(input$evaltype == "glm") {
      if(input$glmfamily == "gaussian") {
        return(input$snr)
      }
      if(input$glmfamily == "binomial") {
        return(c(input$binomialprobs[1],input$binomialprobs[2]))
      }
      if(input$glmfamily == "poisson") {
        return((c(input$poislow,input$poishigh)))
      }
      if(input$glmfamily == "exponential") {
        return(c(input$explow,input$exphigh))
      }
    }
    if(input$evaltype == "surv") {
      if(input$distribution == "gaussian") {
        return(input$snr)
      }
      if(input$distribution == "lognormal") {
        return(input$snr)
      }
      if(input$distribution == "exponential") {
        return(c(input$explow,input$exphigh))
      }
    }
  })

  runmatrix = reactive({
    input$submitbutton
    if(isolate(input$setseed)) {
      set.seed(isolate(input$seed))
    }
    if(isolate(input$parallel)) {
      showNotification("Searching (no progress bar with multicore on):",type="message")
    }
    if(isblocking() && isolate(input$optimality) %in% c("Alias","T","G")) {
      print("Hard-to-change factors are not currently supported for Alias, T, and G optimal designs.")
    } else {

      if(!isblocking()) {
        if(isolate(as.logical(input$parallel))) {
          gen_design(candidateset = isolate(expand.grid(candidatesetall())),
                     model = isolate(as.formula(input$model)),
                     trials = isolate(input$trials),
                     optimality = isolate(optimality()),
                     repeats = isolate(input$repeats),
                     aliaspower = isolate(input$aliaspower),
                     minDopt = isolate(input$mindopt),
                     parallel = isolate(as.logical(input$parallel)))
        } else {
          withProgress(message = "Generating design:", value=0, min = 0, max = 1, expr = {
            gen_design(candidateset = isolate(expand.grid(candidatesetall())),
                       model = isolate(as.formula(input$model)),
                       trials = isolate(input$trials),
                       optimality = isolate(optimality()),
                       repeats = isolate(input$repeats),
                       aliaspower = isolate(input$aliaspower),
                       minDopt = isolate(input$mindopt),
                       parallel = isolate(as.logical(input$parallel)),
                       progressBarUpdater = incProgressSession)})
        }
      } else {
        spd = gen_design(candidateset = isolate(expand.grid(candidatesetall())),
                         model = isolate(as.formula(blockmodel())),
                         trials = isolate(input$numberblocks),
                         optimality = isolate(optimality()),
                         repeats = isolate(input$repeats),
                         varianceratio = isolate(input$varianceratio),
                         aliaspower = isolate(input$aliaspower),
                         minDopt = isolate(input$mindopt),
                         parallel = isolate(as.logical(input$parallel)))
        if(isolate(input$trials) %% isolate(input$numberblocks) == 0) {
          sizevector = isolate(input$trials)/isolate(input$numberblocks)
        } else {
          sizevector = c(rep(ceiling(isolate(input$trials)/isolate(input$numberblocks)),isolate(input$numberblocks)))
          unbalancedruns = ceiling(isolate(input$trials)/isolate(input$numberblocks))*isolate(input$numberblocks) - isolate(input$trials)
          sizevector[(length(sizevector)-unbalancedruns+1):length(sizevector)] = sizevector[(length(sizevector)-unbalancedruns+1):length(sizevector)] - 1
        }
        if(isolate(as.logical(input$parallel))) {
          gen_design(candidateset = isolate(expand.grid(candidatesetall())),
                     model = isolate(as.formula(input$model)),
                     trials = isolate(input$trials),
                     splitplotdesign = spd,
                     splitplotsizes = sizevector,
                     optimality = isolate(optimality()),
                     repeats = isolate(input$repeats),
                     varianceratio = isolate(input$varianceratio),
                     aliaspower = isolate(input$aliaspower),
                     minDopt = isolate(input$mindopt),
                     parallel = isolate(as.logical(input$parallel)),
                     splitcolumns = isolate(input$splitanalyzable))
        } else {
          withProgress(message = "Generating design", value=0, min = 0, max = 1, expr = {
            gen_design(candidateset = isolate(expand.grid(candidatesetall())),
                       model = isolate(as.formula(input$model)),
                       trials = isolate(input$trials),
                       splitplotdesign = spd,
                       splitplotsizes = sizevector,
                       optimality = isolate(optimality()),
                       repeats = isolate(input$repeats),
                       varianceratio = isolate(input$varianceratio),
                       aliaspower = isolate(input$aliaspower),
                       minDopt = isolate(input$mindopt),
                       parallel = isolate(as.logical(input$parallel)),
                       splitcolumns = isolate(input$splitanalyzable),
                       progressBarUpdater = incProgressSession)})
        }
      }
    }
  })

  evaluationtype = reactive({
    input$evalbutton
    isolate(input$evaltype)
  })

  powerresults = reactive({
    input$evalbutton
    if(isblocking() && isolate(optimality()) %in% c("Alias","T","G")) {
      print("No design generated")
    } else {
      if(evaluationtype() == "lm") {
        eval_design(RunMatrix = isolate(runmatrix()),
                    model = as.formula(isolate(input$model)),
                    alpha = isolate(input$alpha),
                    blocking = isblocking(),
                    effectsize = isolate(effectsize()),
                    conservative = isolate(input$conservative),
                    detailedoutput = isolate(input$detailedoutput))
      }
    }
  })
  powerresultsglm = reactive({
    input$evalbutton
    if(isolate(input$setseed)) {
      set.seed(isolate(input$seed))
    }
    if(isolate(input$parallel_eval_glm)) {
      showNotification("Simulating (no progress bar with multicore on):",type="message")
    }
    if(isblocking() && isolate(optimality()) %in% c("Alias","T","G")) {
      print("No design generated")
    } else {
      if(evaluationtype() == "glm") {
        if(isolate(input$parallel_eval_glm)) {
          eval_design_mc(RunMatrix = isolate(runmatrix()),
                         model = isolate(as.formula(input$model)),
                         alpha = isolate(input$alpha),
                         blocking = isblocking(),
                         nsim = isolate(input$nsim),
                         varianceratios = isolate(input$varianceratio),
                         glmfamily = isolate(input$glmfamily),
                         effectsize = isolate(effectsize()),
                         parallel = isolate(input$parallel_eval_glm),
                         detailedoutput = isolate(input$detailedoutput))
        } else {
          withProgress(message = ifelse(isblocking(),"Simulating (with REML):","Simulating:"), value=0, min = 0, max = 1, expr = {
            eval_design_mc(RunMatrix = isolate(runmatrix()),
                           model = isolate(as.formula(input$model)),
                           alpha = isolate(input$alpha),
                           blocking = isblocking(),
                           nsim = isolate(input$nsim),
                           varianceratios = isolate(input$varianceratio),
                           glmfamily = isolate(input$glmfamily),
                           effectsize = isolate(effectsize()),
                           parallel = isolate(input$parallel_eval_glm),
                           detailedoutput = isolate(input$detailedoutput),
                           progressBarUpdater = incProgressSession)})
        }
      }
    }
  })
  powerresultssurv = reactive({
    input$evalbutton
    if(isolate(input$setseed)) {
      set.seed(isolate(input$seed))
    }
    if(isolate(input$parallel_eval_glm)) {
      showNotification("Simulating (no progress bar with multicore on):",type="message")
    }
    if(isblocking()) {
      print("Hard-to-change factors are not supported for survival designs. Evaluating design with no blocking.")
    }
    if(isblocking() && isolate(optimality()) %in% c("Alias","T","G")) {
      print("No design generated")
    } else {
      if(evaluationtype() == "surv") {
        if(isolate(input$parallel_eval_glm)) {
          eval_design_survival_mc(RunMatrix = isolate(runmatrix()),
                                  model = isolate(as.formula(input$model)),
                                  alpha = isolate(input$alpha),
                                  nsim = isolate(input$nsim),
                                  censorpoint = isolate(input$censorpoint),
                                  censortype = isolate(input$censortype),
                                  distribution = isolate(input$distribution),
                                  effectsize = isolate(effectsize()),
                                  detailedoutput = isolate(input$detailedoutput),
                                  parallel = isolate(input$parallel_eval_glm))
        } else {
          withProgress(message = "Simulating:", value=0, min = 0, max = 1, expr = {
            eval_design_survival_mc(RunMatrix = isolate(runmatrix()),
                                    model = isolate(as.formula(input$model)),
                                    alpha = isolate(input$alpha),
                                    nsim = isolate(input$nsim),
                                    censorpoint = isolate(input$censorpoint),
                                    censortype = isolate(input$censortype),
                                    distribution = isolate(input$distribution),
                                    effectsize = isolate(effectsize()),
                                    detailedoutput = isolate(input$detailedoutput),
                                    parallel = isolate(input$parallel_eval_glm),
                                    progressBarUpdater = incProgressSession)
          })
        }
      }
    }
  })


  output$runmatrix = renderTable({
    runmatrix()
  },rownames=TRUE, bordered=TRUE,hover=TRUE,align="c")

  output$powerresults = renderTable({
    powerresults()
  },digits=4,hover=TRUE,align="c")

  output$powerresultsglm = renderTable({
    powerresultsglm()
  },digits=4,hover=TRUE,align="c")

  output$powerresultssurv = renderTable({
    powerresultssurv()
  },digits=4,hover=TRUE,align="c")

  output$aliasplot = renderPlot({
    input$submitbutton
    if(isolate(isblocking()) && isolate(optimality()) %in% c("Alias","T","G")) {
      print("No design generated")
    } else {
      tryCatch({
        plot_correlations(isolate(runmatrix()))
      }, error = function(e) {
      })
    }
  })

  output$fdsplot = renderPlot({
    input$submitbutton
    if(isolate(isblocking()) && isolate(optimality()) %in% c("Alias","T","G")) {
      print("No design generated")
    } else {
      plot_fds(isolate(runmatrix()))
    }
  })

  output$code = renderUI({
    HTML(code())
  })

  output$dopt = renderText({
    input$submitbutton
    isolate(attr(runmatrix(),"D"))
  })
  output$aopt = renderText({
    input$submitbutton
    isolate(attr(runmatrix(),"A"))
  })
  output$iopt = renderText({
    input$submitbutton
    isolate(attr(runmatrix(),"I"))
  })
  output$eopt = renderText({
    input$submitbutton
    isolate(attr(runmatrix(),"E"))
  })
  output$gopt = renderText({
    input$submitbutton
    isolate(attr(runmatrix(),"G"))
  })
  output$topt = renderText({
    input$submitbutton
    isolate(attr(runmatrix(),"T"))
  })
  output$optimalsearch = renderPlot({
    input$submitbutton
    if(isolate(optimality()) %in% c("D","G","A")) {
      isolate(plot(attr(runmatrix(),"optimalsearchvalues"),xlab="Search Iteration",ylab=paste(optimality(), "Efficiency (higher is better)"),type = 'p', col = 'red', pch=16,ylim=c(0,100)))
      isolate(points(x=attr(runmatrix(),"best"),y=attr(runmatrix(),"optimalsearchvalues")[attr(runmatrix(),"best")],type = 'p', col = 'green', pch=16,cex =2,ylim=c(0,100)))
    } else {
      if(isolate(optimality()) == "I") {
        isolate(plot(attr(runmatrix(),"optimalsearchvalues"),xlab="Search Iteration",ylab="Average Prediction Variance (lower is better)",type = 'p', col = 'red', pch=16))
      } else {
        isolate(plot(attr(runmatrix(),"optimalsearchvalues"),xlab="Search Iteration",ylab=paste(optimality(), "Criteria Value (higher is better)"),type = 'p', col = 'red', pch=16))
      }
      isolate(points(x=attr(runmatrix(),"best"),y=attr(runmatrix(),"optimalsearchvalues")[attr(runmatrix(),"best")],type = 'p', col = 'green', pch=16,cex =2))
    }
  })
  output$simulatedpvalues = renderPlot({
    input$evalbutton
    pvalrows = isolate(floor(ncol(attr(powerresultsglm(),"pvals"))/3)+1)
    if(!is.null(attr(powerresultsglm(),"pvals"))) {
      par(mfrow=c(pvalrows,3))
      for(col in 1:isolate(ncol(attr(powerresultsglm(),"pvals")))) {
        isolate(hist(attr(powerresultsglm(),"pvals")[,col],breaks=seq(0,1,0.05),main = colnames(attr(powerresultsglm(),"pvals"))[col],xlim=c(0,1),xlab="p values",ylab="Count", col = 'red', pch=16))
      }
    }
  })
  output$parameterestimates = renderPlot({
    input$evalbutton
    if(!is.null(attr(powerresultsglm(),"estimates"))) {
      ests = apply(attr(powerresultsglm(),"estimates"),2,quantile,c(0.05,0.5,0.95))
      truth = attr(powerresultsglm(),"anticoef")
      if(isolate(input$glmfamily) == "binomial") {
        ests = exp(ests)/(1+exp(ests))
        truth = exp(truth)/(1+exp(truth))
      }
      if(isolate(input$glmfamily) == "poisson") {
        ests = exp(ests)
        truth = exp(truth)
      }
      if(isolate(input$glmfamily) == "exponential") {
        ests = exp(ests)
        truth = exp(truth)
      }
      par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
      plot(x=1:length(colnames(ests)),y=ests[2,],
           xaxt = "n",
           xlab = "Parameters",
           ylab = ifelse(isolate(input$glmfamily) == "binomial", "Parameter Estimates (Probability)","Parameter Estimates"),
           ylim = ifelse(rep(isolate(input$glmfamily) == "binomial",2), c(0,1), c(min(as.vector(ests)),max(as.vector(ests)))),
           xlim = c(0.5,length(colnames(ests))+0.5),
           type = "p",pch = 16,col = "red",cex = 1)
      axis(1,at=1:length(colnames(ests)),labels=colnames(ests), las = 2)
      legend("topright", inset=c(-0.2,0), legend=c("Truth","Simulated"), pch=c(16,16),col=c("blue","red"), title="Estimates")
      par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=FALSE)
      grid(nx=NA,ny=NULL)
      arrows(x0=1:length(colnames(ests)),y0=ests[1,],x1=1:length(colnames(ests)),y1=ests[3,],length=0.05,angle=90,code=3)
      points(x=1:length(colnames(ests)),y=truth,pch=16,col="blue",cex=1)
      title("Simulated Parameter Estimates (5%-95% Confidence Intervals)")
    }
  })

  output$parameterestimatessurv = renderPlot({
    input$evalbutton
    if(!is.null(attr(powerresultssurv(),"estimates"))) {
      ests = apply(attr(powerresultssurv(),"estimates"),2,quantile,c(0.05,0.5,0.95))
      truth = attr(powerresultssurv(),"anticoef")
      if(isolate(input$distibution) == "exponential") {
        ests = exp(ests)
        truth = exp(truth)
      }
      par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
      plot(x=1:length(colnames(ests)),y=ests[2,],
           xaxt = "n",
           xlab = "Parameters",
           ylab = ifelse(isolate(input$glmfamily) == "binomial", "Parameter Estimates (Probability)","Parameter Estimates"),
           ylim = ifelse(rep(isolate(input$glmfamily) == "binomial",2), c(0,1), c(min(as.vector(ests)),max(as.vector(ests)))),
           xlim = c(0.5,length(colnames(ests))+0.5),
           type = "p",pch = 16,col = "red",cex = 1)
      axis(1,at=1:length(colnames(ests)),labels=colnames(ests), las = 2)
      legend("topright", inset=c(-0.2,0), legend=c("Truth","Simulated"), pch=c(16,16),col=c("blue","red"), title="Estimates")
      par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=FALSE)
      grid(nx=NA,ny=NULL)
      arrows(x0=1:length(colnames(ests)),y0=ests[1,],x1=1:length(colnames(ests)),y1=ests[3,],length=0.05,angle=90,code=3)
      points(x=1:length(colnames(ests)),y=truth,pch=16,col="blue",cex=1)
      title("Simulated Parameter Estimates (5%-95% Confidence Intervals)")
    }
  })

  output$responsehistogram = renderPlot({
    input$evalbutton
    if(!is.null(attr(powerresultsglm(),"estimates"))) {
      responses = as.vector(attr(powerresultsglm(),"estimates") %*% t(attr(powerresultsglm(),"modelmatrix")))
      trueresponses = as.vector(attr(powerresultsglm(),"anticoef") %*% t(attr(powerresultsglm(),"modelmatrix")))
      widths = hist(trueresponses,plot=FALSE)$counts
      widths = widths[widths != 0]
      widths= sqrt(widths)
      uniquevalues = length(table(responses))
      breakvalues = ifelse(uniquevalues < isolate(input$nsim)*isolate(input$trials)/10,uniquevalues,isolate(input$nsim)*isolate(input$trials)/10)
      if(isolate(input$glmfamily) == "binomial") {
        responses = exp(responses)/(1+exp(responses))
        trueresponses = exp(trueresponses)/(1+exp(trueresponses))
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
        hist(responses,breaks=breakvalues,xlab="Response (Probability)",main="Distribution of Simulated Response Estimates",xlim=c(0,1))
        legend("topright", inset=c(-0.2,0), legend=c("Truth","Simulated"), pch=c(16,16),col=c("blue","red"), title="Estimates")
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=FALSE)
        grid(nx=NA,ny=NULL)
        hist(responses,breaks=breakvalues,add=TRUE,main="Distribution of Simulated Responses Estimates",xlab="Response",ylab="Count",col = "red",border="red")
        abline(v=unique(trueresponses)[order(unique(trueresponses))],col=adjustcolor("blue",alpha.f=0.40) ,lwd=widths)
      }
      if(isolate(input$glmfamily) == "poisson") {
        responses = exp(responses)
        trueresponses = exp(trueresponses)
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
        hist(responses,breaks=breakvalues,xlab="Response",main="Distribution of Simulated Response Estimates",xlim=c(ifelse(is.na(input$estimatesxminglm),min(hist(responses,plot=FALSE)$breaks),(input$estimatesxminglm)),ifelse(is.na(input$estimatesxmaxglm),max(hist(responses,plot=FALSE)$breaks),(input$estimatesxmaxglm))),col = "red",border="red")
        legend("topright", inset=c(-0.2,0), legend=c("Truth","Simulated"), pch=c(16,16),col=c("blue","red"), title="Estimates")
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=FALSE)
        grid(nx=NA,ny=NULL)
        hist(responses,breaks=breakvalues,add=TRUE,main="Distribution of Simulated Responses ",xlab="Response",ylab="Count",col = "red",border="red")
        abline(v=unique(trueresponses)[order(unique(trueresponses))],col=adjustcolor("blue",alpha.f=0.40), lwd=widths)
      }
      if(isolate(input$glmfamily) == "exponential") {
        responses = exp(responses)
        trueresponses = exp(trueresponses)
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
        hist(responses,breaks=breakvalues,xlab="Response",main="Distribution of Simulated Response Estimates",xlim=c(ifelse(is.na(input$estimatesxminglm),min(hist(responses,plot=FALSE)$breaks),(input$estimatesxminglm)),ifelse(is.na(input$estimatesxmaxglm),max(hist(responses,plot=FALSE)$breaks),(input$estimatesxmaxglm))),col = "red",border="red")
        legend("topright", inset=c(-0.2,0), legend=c("Truth","Simulated"), pch=c(16,16),col=c("blue","red"), title="Estimates")
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=FALSE)
        grid(nx=NA,ny=NULL)
        hist(responses,breaks=breakvalues,add=TRUE,main="Distribution of Simulated Responses",xlab="Response",ylab="Count",col = "red",border="red")
        abline(v=unique(trueresponses)[order(unique(trueresponses))],col=adjustcolor("blue",alpha.f=0.40), lwd=widths)
      }
      if(isolate(input$glmfamily) == "gaussian") {
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
        hist(responses,breaks=breakvalues,xlab="Response",main="Distribution of Simulated Response Estimates",xlim=c(ifelse(is.na(input$estimatesxminglm),min(hist(responses,plot=FALSE)$breaks),(input$estimatesxminglm)),ifelse(is.na(input$estimatesxmaxglm),max(hist(responses,plot=FALSE)$breaks),(input$estimatesxmaxglm))),col = "red",border="red")
        legend("topright", inset=c(-0.2,0), legend=c("Truth","Simulated"), pch=c(16,16),col=c("blue","red"), title="Estimates")
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=FALSE)
        grid(nx=NA,ny=NULL)
        hist(responses,breaks=breakvalues,add=TRUE,main="Distribution of Simulated Responses",xlab="Response",ylab="Count",col = "red",border="red")
        abline(v=unique(trueresponses)[order(unique(trueresponses))],col=adjustcolor("blue",alpha.f=0.40), lwd=widths)

      }
    }
  })

  output$responsehistogramsurv = renderPlot({
    input$evalbutton
    if(!is.null(attr(powerresultssurv(),"estimates"))) {
      responses = as.vector(attr(powerresultssurv(),"estimates") %*% t(attr(powerresultssurv(),"modelmatrix")))
      trueresponses = as.vector(attr(powerresultssurv(),"anticoef") %*% t(attr(powerresultssurv(),"modelmatrix")))
      widths = hist(trueresponses,plot=FALSE)$counts
      widths = widths[widths != 0]
      widths= sqrt(widths)
      uniquevalues = length(table(responses))
      breakvalues = ifelse(uniquevalues < isolate(input$nsim)*isolate(input$trials)/10,uniquevalues,isolate(input$nsim)*isolate(input$trials)/10)
      if(isolate(input$distribution) == "exponential") {
        responses = exp(responses)
        trueresponses = exp(trueresponses)
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
        hist(responses,breaks=breakvalues,xlab="Response",main="Distribution of Simulated Response Estimates",xlim=c(ifelse(is.na(input$estimatesxminsurv),min(hist(responses,plot=FALSE)$breaks),(input$estimatesxminsurv)),ifelse(is.na(input$estimatesxmaxsurv),max(hist(responses,plot=FALSE)$breaks),(input$estimatesxmaxsurv))),col = "red",border="red")
        legend("topright", inset=c(-0.2,0), legend=c("Truth","Simulated"), pch=c(16,16),col=c("blue","red"), title="Estimates")
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=FALSE)
        grid(nx=NA,ny=NULL)
        hist(responses,breaks=breakvalues,add=TRUE,main="Distribution of Simulated Responses",xlab="Response",ylab="Count",col = "red",border="red")
        abline(v=unique(trueresponses)[order(unique(trueresponses))],col=adjustcolor("blue",alpha.f=0.40), lwd=widths)
      }
      if(isolate(input$distribution) %in% c("gaussian", "lognormal")) {
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
        hist(responses,breaks=breakvalues,xlab="Response",main="Distribution of Simulated Response Estimates (from survival analysis)",xlim=c(ifelse(is.na(input$estimatesxminsurv),min(hist(responses,plot=FALSE)$breaks),(input$estimatesxminsurv)),ifelse(is.na(input$estimatesxmaxsurv),max(hist(responses,plot=FALSE)$breaks),(input$estimatesxmaxsurv))),col = "red",border="red")
        legend("topright", inset=c(-0.2,0), legend=c("Truth","Simulated"), pch=c(16,16),col=c("blue","red"), title="Estimates")
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=FALSE)
        grid(nx=NA,ny=NULL)
        hist(responses,breaks=breakvalues,add=TRUE,main="Distribution of Simulated Responses",xlab="Response",ylab="Count",col = "red",border="red")
        abline(v=unique(trueresponses)[order(unique(trueresponses))],col=adjustcolor("blue",alpha.f=0.40), lwd=widths)
      }
    }
  })
  output$separationwarning = renderText({
    input$evalbutton
    likelyseparation = FALSE
    if(isolate(input$evaltype) == "glm" && isolate(input$glmfamily) == "binomial") {
      if(!is.null(attr((powerresultsglm()),"pvals"))) {
        pvalmat = attr((powerresultsglm()),"pvals")
        for(i in 2:ncol(pvalmat)) {
          pvalcount = hist(pvalmat[,i],breaks=seq(0,1,0.05),plot=FALSE)
          likelyseparation = likelyseparation || (all(pvalcount$count[20] > pvalcount$count[17:19]) && pvalcount$count[20] > isolate(input$nsim)/15)
        }
      }
    }
    if(likelyseparation) {
      showNotification("Partial or complete separation likely detected in the binomial Monte Carlo simulation. Increase the number of runs in the design or decrease the number of model parameters to improve power.", type="warning",duration=10)
    }
  })
  observeEvent(input$tutorial,
               introjs(session,
                       options = list("showProgress" = 'true',
                                      "showBullets" = 'false'),
                       events = list(
                         "onchange" = I("
                                        if (this._currentStep==0) {
                                        $('a[data-value=\"Advanced\"]').removeClass('active');
                                        $('a[data-value=\"Power\"]').removeClass('active');
                                        $('a[data-value=\"Basic\"]').addClass('active');
                                        $('a[data-value=\"Basic\"]').trigger('click');
                                        }
                                        if (this._currentStep==5) {
                                        $('a[data-value=\"Power\"]').removeClass('active');
                                        $('a[data-value=\"Basic\"]').removeClass('active');
                                        $('a[data-value=\"Advanced\"]').addClass('active');
                                        $('a[data-value=\"Advanced\"]').trigger('click');
                                        }
                                        if (this._currentStep==13) {
                                        $('a[data-value=\"Advanced\"]').removeClass('active');
                                        $('a[data-value=\"Power\"]').addClass('active');
                                        $('a[data-value=\"Power\"]').trigger('click');
                                        }
                                        if (this._currentStep==17) {
                                        $('input[value=\"glm\"]').trigger('click');
                                        }
                                        if (this._currentStep==21) {
                                        $('input[value=\"surv\"]').trigger('click');
                                        }
                                        if (this._currentStep==24) {
                                        $('a[data-value=\"Design Evaluation\"]').removeClass('active');
                                        $('a[data-value=\"Generating Code\"]').removeClass('active');
                                        $('a[data-value=\"Design\"]').addClass('active');
                                        $('a[data-value=\"Design\"]').trigger('click');
                                        $('#evaltype').val('glm');
                                        Shiny.onInputChange('evaltype','glm');
                                        $('#numberfactors').val('3');
                                        Shiny.onInputChange('numberfactors',3);
                                        $('#trials').val('12');
                                        Shiny.onInputChange('trials',12);
                                        $('#submitbutton').trigger('click');
                                        $('#evalbutton').trigger('click');
                                        }
                                        if (this._currentStep==25) {
                                        $('#evalbutton').trigger('click');
                                        $('a[data-value=\"Design\"]').removeClass('active');
                                        $('a[data-value=\"Design Evaluation\"]').addClass('active');
                                        $('a[data-value=\"Design Evaluation\"]').trigger('click');
                                        }
                                        if (this._currentStep==31) {
                                        $('a[data-value=\"Design Evaluation\"]').removeClass('active');
                                        $('a[data-value=\"Generating Code\"]').addClass('active');
                                        $('a[data-value=\"Generating Code\"]').trigger('click');
                                        }"
                  ))
                ))
    outputOptions(output,"separationwarning", suspendWhenHidden=FALSE)
}
)
