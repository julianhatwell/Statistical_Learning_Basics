# use this to do EDA, as well as the marvelous EDA report
par(ask = TRUE)
for (var in dt$vars[!dt$vars_fac]) {
  v <- myViolinPlot(var, dt$dt.frm, dt$resp)
  print(v)
}
for (var in dt$vars[!dt$vars_fac]) {
  v <- myScatterPlot(var, dt$dt.frm, dt$resp)
  print(v)
}
for (var in dt$vars[!dt$vars_fac]) {
  v <- myDensityPlot(var, dt$dt.frm, dt$resp, pnts = FALSE)
  print(v)
}
par(ask = FALSE)

