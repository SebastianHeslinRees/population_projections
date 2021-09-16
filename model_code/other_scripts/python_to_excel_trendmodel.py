import pandas as pd
import openpyxl

def python_to_excel_trendmodel(persons, females, males, components, output_dir, wb_filename):

  #read in the template
  book = openpyxl.load_workbook('input_data/excel_templates/trend_template_2020.xlsx')
  
  #Set the output name and prep the file for writing
  writer = pd.ExcelWriter(output_dir+"datastore/"+wb_filename, engine='openpyxl') 
  writer.book = book
  writer.sheets = dict((ws.title, ws) for ws in book.worksheets)

  #Write the dataframes to the workbook
  persons.to_excel(writer, "persons", index=False)
  females.to_excel(writer, "females", index=False)
  males.to_excel(writer, "males", index=False)
  components.to_excel(writer, "components of change", index=False)
  
  # Format the header row - no border, left aligned
  side = openpyxl.styles.Side(border_style=None)
  no_border = openpyxl.styles.borders.Border(
    left=side, right=side, top=side, bottom=side,
  )

  for ws in book.worksheets:
    for cell in ws["1:1"]:
      cell.border = no_border
      cell.alignment = openpyxl.styles.Alignment(horizontal='left')
  
  # Save the workbook
  writer.save()
