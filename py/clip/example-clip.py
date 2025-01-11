import torch
import clip
from PIL import Image

device = "mps" if torch.backends.mps.is_available() else "cpu"  # MPS = Metal Performance Shaders
model, preprocess = clip.load("ViT-B/32", device=device)

image = preprocess(Image.open("test.png")).unsqueeze(0).to(device)
text = clip.tokenize(["a dark alley", "a sunny beach"]).to(device)

with torch.no_grad():
    logits_per_image, logits_per_text = model(image, text)
    probs = logits_per_image.softmax(dim=-1).cpu().numpy()

print("Label probabilities:", probs)
