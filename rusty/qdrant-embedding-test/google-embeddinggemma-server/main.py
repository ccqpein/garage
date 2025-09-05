import numpy as np
import torch
from fastapi import FastAPI
from pydantic import BaseModel
from transformers import AutoModel, AutoTokenizer

app = FastAPI()

# Load model and tokenizer globally for efficiency
# Depending on your download method, 'google/embeddinggemma-300m' will either
# load from the Hugging Face cache or you can point it to your cloned directory.
model_name_or_path = "google/embeddinggemma-300m"

tokenizer = AutoTokenizer.from_pretrained(model_name_or_path)
model = AutoModel.from_pretrained(model_name_or_path)

# Determine device (GPU if available, otherwise CPU)
device = "cuda" if torch.cuda.is_available() else "cpu"
model.to(device)
model.eval()  # Set model to evaluation mode


# Define the request body structure
class EmbeddingRequest(BaseModel):
    texts: list[str]


# Mean Pooling - Take attention mask into account for correct averaging
def mean_pooling(model_output, attention_mask):
    token_embeddings = model_output[
        0
    ]  # First element of model_output contains all token embeddings
    input_mask_expanded = (
        attention_mask.unsqueeze(-1).expand(token_embeddings.size()).float()
    )
    sum_embeddings = torch.sum(token_embeddings * input_mask_expanded, 1)
    sum_mask = torch.clamp(input_mask_expanded.sum(1), min=1e-9)
    return sum_embeddings / sum_mask


@app.post("/embed")
async def get_embeddings(request: EmbeddingRequest):
    if not request.texts:
        return {"embeddings": []}

    # Tokenize input texts
    encoded_input = tokenizer(
        request.texts, padding=True, truncation=True, return_tensors="pt"
    ).to(device)

    # Compute token embeddings
    with torch.no_grad():
        model_output = model(**encoded_input)

    # Perform mean pooling
    sentence_embeddings = mean_pooling(model_output, encoded_input["attention_mask"])

    # Normalize embeddings (important for many embedding models)
    sentence_embeddings = torch.nn.functional.normalize(sentence_embeddings, p=2, dim=1)

    # Convert to list for JSON response
    embeddings_list = sentence_embeddings.cpu().numpy().tolist()

    return {"embeddings": embeddings_list}


if __name__ == "__main__":
    import uvicorn

    # Run the server on all available interfaces on port 8000
    uvicorn.run(app, host="0.0.0.0", port=9527)
