Attribute VB_Name = "datastoreVBA_households"
Sub run_it()
Set This_wkb = Application.ThisWorkbook
Set wkb = Workbooks.Add
Call run_open
Application.DisplayAlerts = False
Worksheets("Sheet1").Delete
Application.DisplayAlerts = True
wkb.SaveAs fileName:="M:\Projects\population_projections\Q:\Teams\D&PA\Demography\Projections\2018_draft_outputs\2018_central\dclg_households"
wkb.Close SaveChanges:=False
End Sub
Sub run_open()
Call open_copy_csv("Q:/Teams/D&PA/Demography/Projections/2018_draft_outputs/2018_central/households\dclg_stage1_households.csv", "stage 1")
Call open_copy_csv("Q:/Teams/D&PA/Demography/Projections/2018_draft_outputs/2018_central/households\dclg_stage2_households.csv", "stage 2")
Call open_copy_csv("Q:/Teams/D&PA/Demography/Projections/2018_draft_outputs/2018_central/households\dclg_detailed_hh_pop.csv", "household popn")
Call open_copy_csv("Q:/Teams/D&PA/Demography/Projections/2018_draft_outputs/2018_central/households\dclg_detailed_ce_pop.csv", "communal est popn")
Call open_copy_csv("Q:/Teams/D&PA/Demography/Projections/2018_draft_outputs/2018_central/households\dclg_household_summary.csv", "summary")
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
