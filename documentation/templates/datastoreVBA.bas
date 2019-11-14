Attribute VB_Name = "datastoreVBA"
Sub run_it()
Set This_wkb = Application.ThisWorkbook
Set wkb = Workbooks.Add
Call run_open
Application.DisplayAlerts = False
Worksheets("Sheet1").Delete
Application.DisplayAlerts = True
wkb.SaveAs fileName:="M:\Projects\population_projections\outputs\trend\2018\2018_long\datastore_19-11-13_2144\2018_long_trend.xlsx"
wkb.Close SaveChanges:=False
End Sub
Sub run_open()
Call open_copy_csv("M:\Projects\population_projections\outputs\trend\2018\2018_long\datastore_19-11-13_2144\persons.csv", "persons")
Call open_copy_csv("M:\Projects\population_projections\outputs\trend\2018\2018_long\datastore_19-11-13_2144\females.csv", "females")
Call open_copy_csv("M:\Projects\population_projections\outputs\trend\2018\2018_long\datastore_19-11-13_2144\males.csv", "males")
Call open_copy_csv("M:\Projects\population_projections\outputs\trend\2018\2018_long\datastore_19-11-13_2144\components.csv", "components of change")
End Sub
Sub open_copy_csv(fileName, tabName)
Dim wbkS As Workbook
Dim wshS As Worksheet
Dim wshT As Worksheet
Set wshT = Worksheets.Add(After:=Worksheets(Worksheets.Count))
Set wbkS = Workbooks.Open(fileName:=fileName)
Set wshS = wbkS.Worksheets(1)
wshS.UsedRange.Copy Destination:=wshT.Range("A1")
wshT.Name = tabName
wbkS.Close SaveChanges:=False
End Sub
