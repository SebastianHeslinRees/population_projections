import pandas as pd
from openpyxl import load_workbook

def python_to_excel(output_dir, wb_filename, small_area):
  
  if(small_area == "ward"):
    book = load_workbook('input_data/excel_templates/ward_housing_led_2020_based_template.xlsx')
  elif(small_area == "msoa"):
    book = load_workbook('input_data/excel_templates/msoa_housing_led_2020_based_template.xlsx')
  else:
    return("Error in python_excel_output: small_area must be either ward or msoa")

  persons = pd.read_csv(output_dir+small_area+"/persons_"+small_area+".csv")
  females = pd.read_csv(output_dir+small_area+"/females_"+small_area+".csv")
  males = pd.read_csv(output_dir+small_area+"/males_"+small_area+".csv")
  components = pd.read_csv(output_dir+small_area+"/components_"+small_area+".csv")

  writer = pd.ExcelWriter(output_dir+"excels/"+wb_filename, engine='openpyxl') 
  writer.book = book
  writer.sheets = dict((ws.title, ws) for ws in book.worksheets)

  persons.to_excel(writer, "persons", index=False)
  females.to_excel(writer, "females", index=False)
  males.to_excel(writer, "males", index=False)
  components.to_excel(writer, "components of change", index=False)
  
  writer.save()