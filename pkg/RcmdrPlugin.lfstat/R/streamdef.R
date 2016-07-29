#########################
# Streamflow deficit    #
#########################

streamdefcalc <- function(){
  initializeDialog(title = gettextRcmdr("Streamflow deficit"))

  # construct the four frames, top is created by initializeDialog()
  framePoolingMethod <- tkframe(top)
  frameThreshold <- tkframe(top)
  frameFurtherArgs <- tkframe(top)
  frameFormatOutput <- tkframe(top)


  # this dialog assigns its return value (table of deficits) to an object
  # construct name of this object
  # first assign the default value to the variable, then bind the variable
  # to an tcl/tk frame
  entryGlobalObj <- tclVar(gettextRcmdr(paste0("StrdefTableof", ActiveDataSet())))
  globalObj <- ttkentry(framePoolingMethod, width = "25", textvariable = entryGlobalObj)

  # To ease localization, titles and labels should be fetched by gettext()
  # defines, initializes and displays the buttons
  # generates a an object in the calling environment
  # named 'poolingFrame' (names constructed from arg 'name')
  radioButtons(window = framePoolingMethod,
               name = "pooling",
               buttons = c("none", "MA","IT","IC"),
               labels = gettextRcmdr(c("none", "MA", "IT", "IC")),
               title = gettextRcmdr("Pooling method:"),
               initialValue = getlfopt("strmethod"))




  radioButtons(window = frameThreshold,
               name = "thresbreaks",
               buttons = c("fixed", "monthly", "daily", "seasonal"),
               labels = c("fixed", "monthly", "daily", "seasonal"),
               title = gettextRcmdr("Threshold selection:"),
               initialValue = getlfopt("strthresbreaks"))

  tlevel <- tclVar(gettextRcmdr(getlfopt("strlevel")))
  entrytlevel <- ttkentry(frameThreshold,
                          width = "2", textvariable = tlevel)

  breakseas <- tclVar(gettextRcmdr(getlfopt("season")))
  entrybreakseas <- ttkentry(parent = frameThreshold,
                             width = "25", textvariable = breakseas)




  Madays <- tclVar(gettextRcmdr(getlfopt("strMadays")))
  entrymadays <- ttkentry(frameFurtherArgs, width = "3", textvariable = Madays)

  tmin <- tclVar(gettextRcmdr(getlfopt("strtmin")))
  entrytmin <- ttkentry(frameFurtherArgs, width = "3", textvariable = tmin)

  IClevel <- tclVar(gettextRcmdr(getlfopt("strIClevel")))
  entryIClevel <- ttkentry(frameFurtherArgs, width = "3", textvariable = IClevel)



  radioButtons(frameFormatOutput,
               "table",
               buttons = c("all","volmax", "durmax"),
               labels = gettextRcmdr(c("All deficits",
                                       "Annual extremes (vol)",
                                       "Annual extremes (dur)")),
               title = gettextRcmdr("Streamflow table contains:"),
               initialValue = getlfopt("strtable"))

  mindur <- tclVar(gettextRcmdr(getlfopt("strmindur")))
  entrymindur <- ttkentry(frameFormatOutput, width = "6", textvariable = mindur)

  minvol <- tclVar(gettextRcmdr(getlfopt("strminvol")))
  entryminvol <- ttkentry(frameFormatOutput, width = "15", textvariable = minvol)

  plotDeficit <- tclVar(gettextRcmdr(getlfopt("plotDeficit")))
  entryplotDeficit <- ttkcheckbutton(frameFormatOutput, variable = plotDeficit)



  onOK <- function(){
    pool <- tclvalue(poolingVariable)
    options("RcmdrPlugin.lfstat" =
              modifyList(getOption("RcmdrPlugin.lfstat"),list(strmethod = pool)))

    threshold <- tclvalue(tlevel)
    options("RcmdrPlugin.lfstat" =
              modifyList(getOption("RcmdrPlugin.lfstat"),list(strlevel = as.numeric(threshold))))

    breaks <- tclvalue(thresbreaksVariable)
    options("RcmdrPlugin.lfstat" =
              modifyList(getOption("RcmdrPlugin.lfstat"),list(strthresbreaks = breaks)))

    seas <- tclvalue(breakseas)
    options("RcmdrPlugin.lfstat" =
              modifyList(getOption("RcmdrPlugin.lfstat"),list(season = seas)))

    MAdays <- tclvalue(Madays)
    options("RcmdrPlugin.lfstat" =
              modifyList(getOption("RcmdrPlugin.lfstat"),list(strMadays = as.numeric(MAdays))))

    Tmin <- tclvalue(tmin)
    options("RcmdrPlugin.lfstat" =
              modifyList(getOption("RcmdrPlugin.lfstat"),list(strtmin = as.numeric(Tmin))))

    icLevel <- tclvalue(IClevel)
    options("RcmdrPlugin.lfstat" =
              modifyList(getOption("RcmdrPlugin.lfstat"),list(strIClevel = as.numeric(icLevel))))

    minDur <- tclvalue(mindur)
    options("RcmdrPlugin.lfstat" =
              modifyList(getOption("RcmdrPlugin.lfstat"),list(strmindur = minDur)))

    minVol <- tclvalue(minvol)
    options("RcmdrPlugin.lfstat" =
              modifyList(getOption("RcmdrPlugin.lfstat"),list(strminvol = minVol)))

    table <- tclvalue(tableVariable)
    options("RcmdrPlugin.lfstat" =
              modifyList(getOption("RcmdrPlugin.lfstat"),list(strtable = table)))

    plotDeficit <- tclvalue(plotDeficit)
    options("RcmdrPlugin.lfstat" =
              modifyList(getOption("RcmdrPlugin.lfstat"),list(plotDeficit = plotDeficit)))

    closeDialog()
    nameValue <- trim.blanks(tclvalue(entryGlobalObj))
    if (nameValue == ""){
      errorCondition(recall = readDataSet,
                     message = gettextRcmdr("You must enter a name for the data set."))
      return()
    }
    if (!is.valid.name(nameValue)){
      errorCondition(recall = readDataSet,
                     message = paste('"', nameValue, '" ', gettextRcmdr("is not a valid name."), sep = ""))
      return()
    }
    if (is.element(nameValue, listDataSets())) {
      if ("no" == tclvalue(checkReplace(nameValue, gettextRcmdr("Data set")))){
        readDataSet()
        return()
      }
    }

    command <- paste0("lfstat:::streamdefRcmdr(lfobj = ", ActiveDataSet(),
                      ", pooling = '", pool, "', threslevel = ", threshold,
                      ", thresbreaks = '", breaks, "', breakdays = c(", seas, ")",
                      ", MAdays = ", MAdays, ", tmin = ", Tmin,
                      ", IClevel = ", icLevel,", mindur = '", minDur,
                      "', minvol = '", minVol, "', table = '", table, "'",
                      ", plot = ", plotDeficit, ")")
    doItAndPrint(paste(nameValue, " <- ", command, sep = ""))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "streamdef")

  tkgrid(labelRcmdr(framePoolingMethod, text = gettextRcmdr("Enter name for streamflow table:")), globalObj, sticky = "w")
  tkgrid(framePoolingMethod, sticky = "w")
  # this are the radio-buttons for the pooling method
  tkgrid(poolingFrame, sticky = "w")

  tkgrid(frameThreshold, sticky = "w")
  tkgrid(thresbreaksFrame, sticky = "w")
  tkgrid(labelRcmdr(frameThreshold, text = gettextRcmdr("Threshold: Q")), entrytlevel, sticky = "w")
  tkgrid(labelRcmdr(frameThreshold, text = gettextRcmdr("Breakdays (seasonal only): ")),entrybreakseas , sticky = "w")


  tkgrid(labelRcmdr(frameFurtherArgs, text = ""))
  tkgrid(labelRcmdr(frameFurtherArgs, text = gettextRcmdr("MAdays (MA only): ")), entrymadays,sticky = "w")
  tkgrid(labelRcmdr(frameFurtherArgs, text = gettextRcmdr("tmin (IT, IC only): ")), entrytmin, sticky = "w")
  tkgrid(labelRcmdr(frameFurtherArgs, text = gettextRcmdr("IC-Level (IC only): ")), entryIClevel, sticky = "w")
  tkgrid(frameFurtherArgs, sticky = "w")

  tkgrid(frameFormatOutput, sticky = "w")
  tkgrid(tableFrame, sticky = "w")
  tkgrid(labelRcmdr(frameFormatOutput, text = gettextRcmdr("Only show events with duration > ")), entrymindur ,sticky = "w")
  tkgrid(labelRcmdr(frameFormatOutput, text = gettextRcmdr("Only show events with volume > ")), entryminvol, sticky = "w")
  tkgrid(labelRcmdr(frameFormatOutput, text = gettextRcmdr("Plot Hydrograph with deficit events ")), entryplotDeficit, sticky = "w")


  # OK/ Cancel/ Help buttons
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(rows = 11, columns = 2)
}
