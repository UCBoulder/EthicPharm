from pdf2image import convert_from_path

pdf_path = "Lit_pred_tox_concentration_graphs.pdf"  
images = convert_from_path(pdf_path, dpi=300)

for i, image in enumerate(images):
    image.save(f"page_{i+1}.png", "PNG")
