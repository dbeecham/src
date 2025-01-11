import torch
from transformers import CLIPProcessor, CLIPModel
from PIL import Image

device = "mps" if torch.backends.mps.is_available() else "cuda" if cuda.backends.cuda.is_available() else "cpu"

# Ladda modellen och preprocessorn
model = CLIPModel.from_pretrained(
    "openai/clip-vit-base-patch32",
    device_map=device
)

processor = CLIPProcessor.from_pretrained("openai/clip-vit-base-patch32")

# Ladda bilden
image = Image.open("test.png")

# Processa bilden
inputs = processor(images=image, return_tensors="pt", text=["a photo of ..."])

# Få modellen att välja den bästa texten
outputs = model(**inputs)
logits_per_image = outputs.logits_per_image
probs = logits_per_image.softmax(dim=1)

print("Predicted description:", probs)
