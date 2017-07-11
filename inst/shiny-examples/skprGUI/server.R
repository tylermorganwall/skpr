library(skpr)

shinyServer(
function(input, output) {

  inputlist_htc = reactive({
    input$submitbutton
    inputlist1 = list()
    for(i in 1:6) {
      if((i == 1 && (input$numberfactors) > 0) && isolate(input$blockdepth1) == "htc") {
        if((input$factortype1) == "numeric") {
          inputlist1[[(input$factorname1)]] = seq((input$numericlow1),(input$numerichigh1),length.out = input$numericlength1)
        }
        if((input$factortype1) == "discnum") {
          inputlist1[[(input$factorname1)]] = as.numeric(strsplit((input$disclevels1),split=",")[[1]])
        }
        if((input$factortype1) == "cat") {
          inputlist1[[(input$factorname1)]] = strsplit((input$levels1),split=",")[[1]]
        }
      }
      if((i == 2 && (input$numberfactors) > 1) && isolate(input$blockdepth2) == "htc") {
        if((input$factortype2) == "numeric") {
          inputlist1[[(input$factorname2)]] = seq((input$numericlow2),(input$numerichigh2),length.out = input$numericlength2)
        }
        if((input$factortype2) == "discnum") {
          inputlist1[[(input$factorname2)]] = as.numeric(strsplit((input$disclevels2),split=",")[[1]])
        }
        if((input$factortype2) == "cat") {
          inputlist1[[(input$factorname2)]] = strsplit((input$levels2),split=",")[[1]]
        }
      }
      if((i == 3 && (input$numberfactors) > 2) && isolate(input$blockdepth3) == "htc") {
        if((input$factortype3) == "numeric") {
          inputlist1[[(input$factorname3)]] = seq((input$numericlow3),(input$numerichigh3),length.out = input$numericlength3)
        }
        if((input$factortype3) == "discnum") {
          inputlist1[[(input$factorname3)]] = as.numeric(strsplit((input$disclevels3),split=",")[[1]])
        }
        if((input$factortype3) == "cat") {
          inputlist1[[(input$factorname3)]] = strsplit((input$levels3),split=",")[[1]]
        }
      }
      if((i == 4 && (input$numberfactors) > 3) && isolate(input$blockdepth4) == "htc") {
        if((input$factortype4) == "numeric") {
          inputlist1[[(input$factorname4)]] = seq((input$numericlow4),(input$numerichigh4),length.out = input$numericlength4)
        }
        if((input$factortype4) == "discnum") {
          inputlist1[[(input$factorname4)]] = as.numeric(strsplit((input$disclevels4),split=",")[[1]])
        }
        if((input$factortype4) == "cat") {
          inputlist1[[(input$factorname4)]] = strsplit((input$levels4),split=",")[[1]]
        }
      }
      if((i == 5 && (input$numberfactors) > 4) && isolate(input$blockdepth5) == "htc") {
        if((input$factortype5) == "numeric") {
          inputlist1[[(input$factorname5)]] = seq((input$numericlow5),(input$numerichigh5),length.out = input$numericlength5)
        }
        if((input$factortype5) == "discnum") {
          inputlist1[[(input$factorname5)]] = as.numeric(strsplit((input$disclevels5),split=",")[[1]])
        }
        if((input$factortype5) == "cat") {
          inputlist1[[(input$factorname5)]] = strsplit((input$levels5),split=",")[[1]]
        }
      }
      if((i == 6 && (input$numberfactors) > 5) && isolate(input$blockdepth6) == "htc") {
        if((input$factortype6) == "numeric") {
          inputlist1[[(input$factorname6)]] = seq((input$numericlow6),(input$numerichigh6),length.out = input$numericlength6)
        }
        if((input$factortype6) == "discnum") {
          inputlist1[[(input$factorname6)]] = as.numeric(strsplit((input$disclevels6),split=",")[[1]])
        }
        if((input$factortype6) == "cat") {
          inputlist1[[(input$factorname6)]] = strsplit((input$levels6),split=",")[[1]]
        }
      }
    }
    inputlist1
  })

  inputstring_htc = reactive({
    commacount = sum("htc" == c(input$blockdepth1,input$blockdepth2,input$blockdepth3,input$blockdepth4,input$blockdepth5,input$blockdepth6)[1:input$numberfactors])-1
    finalstring = c()
    for(i in 1:6) {
      if(i == 1 && (input$numberfactors) > 0) {
        if(input$blockdepth1 == "htc") {
          finalstring = c(finalstring,(input$factorname1)," = ")
          if((input$factortype1) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow1),",",(input$numerichigh1),", length.out=",input$numericlength1,")")
          }
          if((input$factortype1) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels1),")")
          }
          if((input$factortype1) == "cat") {
            len = length(strsplit(input$levels1,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels1,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
          if(commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",31)),collapse=""))
          }
        }
      }
      if((i == 2 && (input$numberfactors) > 1)) {
        if(input$blockdepth2 == "htc") {
          finalstring = c(finalstring, input$factorname2," = ")
          if((input$factortype2) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow2),",",(input$numerichigh2),", length.out=",input$numericlength2,")")
          }
          if((input$factortype2) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels2),")")
          }
          if((input$factortype2) == "cat") {
            len = length(strsplit(input$levels2,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels2,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
          if(commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",31)),collapse=""))
          }
        }
      }
      if(i == 3 && (input$numberfactors) > 2) {
        if(input$blockdepth3 == "htc") {
          finalstring = c(finalstring,input$factorname3," = ")
          if((input$factortype3) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow3),",",(input$numerichigh3),", length.out=",input$numericlength3,")")
          }
          if((input$factortype3) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels3),")")
          }
          if((input$factortype3) == "cat") {
            len = length(strsplit(input$levels3,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels3,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
          if(commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",31)),collapse=""))
          }
        }
      }
      if(i == 4 && (input$numberfactors) > 3) {
        if(input$blockdepth4 == "htc") {
          finalstring = c(finalstring,input$factorname4," = ")
          if((input$factortype4) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow4),",",(input$numerichigh4),", length.out=",input$numericlength4,")")
          }
          if((input$factortype4) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels4),")")
          }
          if((input$factortype4) == "cat") {
            len = length(strsplit(input$levels4,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels4,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
          if(commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",31)),collapse=""))
          }
        }
      }
      if(i == 5 && (input$numberfactors) > 4) {
        if(input$blockdepth5 == "htc") {
          finalstring = c(finalstring,input$factorname5," = ")
          if((input$factortype5) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow5),",",(input$numerichigh5),", length.out=",input$numericlength5,")")
          }
          if((input$factortype5) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels5),")")
          }
          if((input$factortype5) == "cat") {
            len = length(strsplit(input$levels5,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels5,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
          if(commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",31)),collapse=""))
          }
        }
      }
      if(i == 6 && (input$numberfactors) > 5) {
        if(input$blockdepth6 == "htc") {
          finalstring = c(finalstring,input$factorname6," = ")
          if((input$factortype6) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow6),",",(input$numerichigh6),", length.out=",input$numericlength6,")")
          }
          if((input$factortype6) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels6),")")
          }
          if((input$factortype6) == "cat") {
            len = length(strsplit(input$levels6,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels6,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
        }
      }
    }
    finalstring
  })


  inputlist = reactive({
    input$submitbutton
    inputlist1 = list()
    for(i in 1:6) {
      if(i == 1 && (input$numberfactors) > 0 ) {
        if(input$blockdepth1 == "etc") {
          if((input$factortype1) == "numeric") {
            inputlist1[[(input$factorname1)]] = seq((input$numericlow1),(input$numerichigh1),length.out = input$numericlength1)
          }
          if((input$factortype1) == "discnum") {
            inputlist1[[(input$factorname1)]] = as.numeric(strsplit((input$disclevels1),split=",")[[1]])
          }
          if((input$factortype1) == "cat") {
            inputlist1[[(input$factorname1)]] = strsplit((input$levels1),split=",")[[1]]
          }
        }
      }
      if(i == 2 && (input$numberfactors) > 1) {
        if(input$blockdepth2 == "etc") {
          if((input$factortype2) == "numeric") {
            inputlist1[[(input$factorname2)]] = seq((input$numericlow2),(input$numerichigh2),length.out = input$numericlength2)
          }
          if((input$factortype2) == "discnum") {
            inputlist1[[(input$factorname2)]] = as.numeric(strsplit((input$disclevels2),split=",")[[1]])
          }
          if((input$factortype2) == "cat") {
            inputlist1[[(input$factorname2)]] = strsplit((input$levels2),split=",")[[1]]
          }
        }
      }
      if(i == 3 && (input$numberfactors) > 2) {
        if(input$blockdepth3 == "etc") {
          if((input$factortype3) == "numeric") {
            inputlist1[[(input$factorname3)]] = seq((input$numericlow3),(input$numerichigh3),length.out = input$numericlength3)
          }
          if((input$factortype3) == "discnum") {
            inputlist1[[(input$factorname3)]] = as.numeric(strsplit((input$disclevels3),split=",")[[1]])
          }
          if((input$factortype3) == "cat") {
            inputlist1[[(input$factorname3)]] = strsplit((input$levels3),split=",")[[1]]
          }
        }
      }
      if(i == 4 && (input$numberfactors) > 3) {
        if(input$blockdepth4 == "etc") {
          if((input$factortype4) == "numeric") {
            inputlist1[[(input$factorname4)]] = seq((input$numericlow4),(input$numerichigh4),length.out = input$numericlength4)
          }
          if((input$factortype4) == "discnum") {
            inputlist1[[(input$factorname4)]] = as.numeric(strsplit((input$disclevels4),split=",")[[1]])
          }
          if((input$factortype4) == "cat") {
            inputlist1[[(input$factorname4)]] = strsplit((input$levels4),split=",")[[1]]
          }
        }
      }
      if(i == 5 && (input$numberfactors) > 4) {
        if(input$blockdepth5 == "etc") {
          if((input$factortype5) == "numeric") {
            inputlist1[[(input$factorname5)]] = seq((input$numericlow5),(input$numerichigh5),length.out = input$numericlength5)
          }
          if((input$factortype5) == "discnum") {
            inputlist1[[(input$factorname5)]] = as.numeric(strsplit((input$disclevels5),split=",")[[1]])
          }
          if((input$factortype5) == "cat") {
            inputlist1[[(input$factorname5)]] = strsplit((input$levels5),split=",")[[1]]
          }
        }
      }
      if(i == 6 && (input$numberfactors) > 5) {
        if(input$blockdepth6 == "etc") {
          if((input$factortype6) == "numeric") {
            inputlist1[[(input$factorname6)]] = seq((input$numericlow6),(input$numerichigh6),length.out = input$numericlength6)
          }
          if((input$factortype6) == "discnum") {
            inputlist1[[(input$factorname6)]] = as.numeric(strsplit((input$disclevels6),split=",")[[1]])
          }
          if((input$factortype6) == "cat") {
            inputlist1[[(input$factorname6)]] = strsplit((input$levels6),split=",")[[1]]
          }
        }
      }
    }
    inputlist1
  })

  inputstring = reactive({
    commacount = sum("etc" == c(input$blockdepth1,input$blockdepth2,input$blockdepth3,input$blockdepth4,input$blockdepth5,input$blockdepth6)[1:input$numberfactors])-1
    finalstring = c()
    for(i in 1:6) {
      if(i == 1 && (input$numberfactors) > 0) {
        if(input$blockdepth1 == "etc") {
          finalstring = c(finalstring,(input$factorname1)," = ")
          if((input$factortype1) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow1),",",(input$numerichigh1),", length.out=",input$numericlength1,")")
          }
          if((input$factortype1) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels1),")")
          }
          if((input$factortype1) == "cat") {
            len = length(strsplit(input$levels1,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels1,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
          if(commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",27)),collapse=""))
          }
        }
      }
      if((i == 2 && (input$numberfactors) > 1)) {
        if(input$blockdepth2 == "etc") {
          finalstring = c(finalstring, input$factorname2," = ")
          if((input$factortype2) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow2),",",(input$numerichigh2),", length.out=",input$numericlength2,")")
          }
          if((input$factortype2) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels2),")")
          }
          if((input$factortype2) == "cat") {
            len = length(strsplit(input$levels2,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels2,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
          if(commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",27)),collapse=""))
          }
        }
      }
      if(i == 3 && (input$numberfactors) > 2) {
        if(input$blockdepth3 == "etc") {
          finalstring = c(finalstring,input$factorname3," = ")
          if((input$factortype3) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow3),",",(input$numerichigh3),", length.out=",input$numericlength3,")")
          }
          if((input$factortype3) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels3),")")
          }
          if((input$factortype3) == "cat") {
            len = length(strsplit(input$levels3,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels3,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
          if(commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",27)),collapse=""))
          }
        }
      }
      if(i == 4 && (input$numberfactors) > 3) {
        if(input$blockdepth4 == "etc") {
          finalstring = c(finalstring,input$factorname4," = ")
          if((input$factortype4) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow4),",",(input$numerichigh4),", length.out=",input$numericlength4,")")
          }
          if((input$factortype4) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels4),")")
          }
          if((input$factortype4) == "cat") {
            len = length(strsplit(input$levels4,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels4,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
          if(commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",27)),collapse=""))
          }
        }
      }
      if(i == 5 && (input$numberfactors) > 4) {
        if(input$blockdepth5 == "etc") {
          finalstring = c(finalstring,input$factorname5," = ")
          if((input$factortype5) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow5),",",(input$numerichigh5),", length.out=",input$numericlength5,")")
          }
          if((input$factortype5) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels5),")")
          }
          if((input$factortype5) == "cat") {
            len = length(strsplit(input$levels5,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels5,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
          if(commacount > 0) {
            commacount = commacount - 1
            finalstring = c(finalstring,paste0(c(",\n",rep("&nbsp;",27)),collapse=""))
          }
        }
      }
      if(i == 6 && (input$numberfactors) > 5) {
        if(input$blockdepth6 == "etc") {
          finalstring = c(finalstring,input$factorname6," = ")
          if((input$factortype6) == "numeric") {
            finalstring = c(finalstring, "seq(",(input$numericlow6),",",(input$numerichigh6),", length.out=",input$numericlength6,")")
          }
          if((input$factortype6) == "discnum") {
            finalstring = c(finalstring, "c(",(input$disclevels6),")")
          }
          if((input$factortype6) == "cat") {
            len = length(strsplit(input$levels6,split=",")[[1]])
            levelstring = paste0(c("\""),strsplit(input$levels6,split=",")[[1]],c("\","),collapse="")
            levelstring = substr(levelstring, 1, nchar(levelstring)-1)
            finalstring = c(finalstring, "c(",levelstring,")")
          }
        }
      }
    }
    finalstring
  })

  code = reactive({
    blocking = any("htc" %in% c(input$blockdepth1,input$blockdepth2,input$blockdepth3,input$blockdepth4,input$blockdepth5,input$blockdepth6)[1:input$numberfactors])
    first = paste0(c("<br><pre>",
                     "<code style=\"color:#468449\"># This is the R code used to generate these results in skpr.</code><br>",
                     "<code style=\"color:#468449\"># Copy this into an R script and rerun to reproduce these results.</code><br><br>",
                     ifelse(input$setseed,
                            paste0("<code style=\"color:#468449\">#Setting random number generator seed:</code><br>",
                                   "set.seed(", input$seed, ")<br><br>"),""),
                     ifelse(blocking,
                            paste0(c("<code style=\"color:#468449\"># Generating Hard-to-change candidate set:</code><br>candidateset_htc = expand.grid(",
                                     inputstring_htc(), ")<br><br>",
                                     "<code style=\"color:#468449\"># Generating design for hard-to-change factors:</code> <br>",
                                     "design_htc = gen_design(candidateset = candidateset_htc, <br>", rep("&nbsp;",24),
                                     "model = ", as.character(blockmodel()), ",<br>", rep("&nbsp;",24),
                                     "trials = ", as.character(input$numberblocks),")<br><br>"),collapse=""),""),
                     "<code style=\"color:#468449\"># Generating candidate set:</code><br>",
                     "candidateset = expand.grid(", inputstring(), ")<br><br>",
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
      if(input$delta != 2) {
        first = paste(c(first, ",<br>", rep("&nbsp;",12),
                        "delta = ",input$delta),collapse = "")
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
      if(input$delta != 2) {
        first = paste(c(first, ",<br>", rep("&nbsp;",15),
                        "delta = ",input$delta),collapse = "")
      }
      if(!is.null(input$varianceratios)) {
        first = paste(c(first, ",<br>", rep("&nbsp;",15),
                        "varianceratios = ",input$varianceratio),collapse = "")
      }
      if((input$binomialprobs[1] != 0.4 || input$binomialprobs[2] != 0.6) && input$glmfamily == "binomial") {
        first = paste(c(first, ",<br>", rep("&nbsp;",15),
                        "binomialprobs = c(",input$binomialprobs[1],", ",input$binomialprobs[2]),collapse = "")
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
      if(!is.na(input$censorpoint)) {
        first = paste(c(first, ",<br>", rep("&nbsp;",24),
                        "censorpoint = ",input$censorpoint),collapse = "")
      }
      if(input$censortype != "right") {
        first = paste(c(first, ",<br>", rep("&nbsp;",24),
                        "censortype = \"",input$censortype,"\""),collapse = "")
      }
      if(input$delta != 2) {
        first = paste(c(first, ",<br>", rep("&nbsp;",24),
                        "delta = ",input$delta),collapse = "")
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
      as.formula(paste0("~",paste(names(inputlist_htc()),collapse=" + ")))
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

  runmatrix = reactive({
    input$submitbutton
    if(isolate(input$setseed)) {
      set.seed(isolate(input$seed))
    }
    if(isblocking() && isolate(input$optimality) %in% c("Alias","T","G")) {
      print("Hard-to-change factors are not currently supported for Alias, T, and G optimal designs.")
    } else {

      if(!isblocking()) {
        gen_design(candidateset = isolate(expand.grid(inputlist())),
                   model = isolate(as.formula(input$model)),
                   trials = isolate(input$trials),
                   optimality = isolate(input$optimality),
                   repeats = isolate(input$repeats),
                   aliaspower = isolate(input$aliaspower),
                   minDopt = isolate(input$mindopt),
                   parallel = isolate(as.logical(input$parallel)))
      } else {
        spd = gen_design(candidateset = isolate(expand.grid(inputlist_htc())),
                         model = isolate(as.formula(blockmodel())),
                         trials = isolate(input$numberblocks),
                         optimality = isolate(input$optimality),
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

        gen_design(candidateset = isolate(expand.grid(inputlist())),
                   model = isolate(as.formula(input$model)),
                   trials = isolate(input$trials),
                   splitplotdesign = spd,
                   splitplotsizes = sizevector,
                   optimality = isolate(input$optimality),
                   repeats = isolate(input$repeats),
                   varianceratio = isolate(input$varianceratio),
                   aliaspower = isolate(input$aliaspower),
                   minDopt = isolate(input$mindopt),
                   parallel = isolate(as.logical(input$parallel)))
      }
    }
  })

  evaluationtype = reactive({
    input$evalbutton
    isolate(input$evaltype)
  })

  powerresults = reactive({
    input$evalbutton
    if(isblocking() && isolate(input$optimality) %in% c("Alias","T","G")) {
      print("No design generated")
    } else {
      if(evaluationtype() == "lm") {
        eval_design(RunMatrix = isolate(runmatrix()),
                    model = as.formula(isolate(input$model)),
                    alpha = isolate(input$alpha),
                    blocking = isblocking(),
                    delta = isolate(input$delta),
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
    if(isblocking() && isolate(input$optimality) %in% c("Alias","T","G")) {
      print("No design generated")
    } else {
      if(evaluationtype() == "glm") {
        eval_design_mc(RunMatrix = isolate(runmatrix()),
                       model = isolate(as.formula(input$model)),
                       alpha = isolate(input$alpha),
                       blocking = isblocking(),
                       nsim = isolate(input$nsim),
                       varianceratios = isolate(input$varianceratio),
                       glmfamily = isolate(input$glmfamily),
                       delta = isolate(input$delta),
                       binomialprobs = isolate(c(input$binomialprobs[1],input$binomialprobs[2])),
                       detailedoutput = isolate(input$detailedoutput))
      }
    }
  })
  powerresultssurv = reactive({
    input$evalbutton
    if(isolate(input$setseed)) {
      set.seed(isolate(input$seed))
    }
    if(isblocking()) {
      print("Hard-to-change factors are not supported for survival designs. Evaluating design with no blocking.")
    }
    if(isblocking() && isolate(input$optimality) %in% c("Alias","T","G")) {
      print("No design generated")
    } else {
      if(evaluationtype() == "surv") {
        eval_design_survival_mc(RunMatrix = isolate(runmatrix()),
                                model = isolate(as.formula(input$model)),
                                alpha = isolate(input$alpha),
                                nsim = isolate(input$nsim),
                                censorpoint = isolate(input$censorpoint),
                                censortype = isolate(input$censortype),
                                distribution = isolate(input$distribution),
                                delta = isolate(input$delta),
                                detailedoutput = isolate(input$detailedoutput))
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
    input$evalbutton
    if(isolate(input$numberfactors) == 1) {
    } else {
      if(isblocking() && isolate(input$optimality) %in% c("Alias","T","G")) {
        print("No design generated")
      } else {
        isolate(plot_correlations(runmatrix()))
      }
    }
  })

  output$fdsplot = renderPlot({
    input$evalbutton
    if(isolate(input$numberfactors) == 1) {
    } else {
      if(isblocking() && isolate(input$optimality) %in% c("Alias","T","G")) {
        print("No design generated")
      } else {
        isolate(plot_fds(runmatrix()))
      }
    }
  })

  output$code = renderUI({
    HTML(code())
  })
}
)
