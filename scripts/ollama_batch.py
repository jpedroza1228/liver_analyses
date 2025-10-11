import pandas as pd
import ollama
import time
import os
from pyhere import here

# --- SETTINGS ---
MODEL = "llama3.2:1b"
BATCH_SIZE = 10
INPUT_FILE = here("prompts.csv")
OUTPUT_FILE = here("responses.csv")

# --- LOAD INPUT ---
prompts = pd.read_csv(INPUT_FILE)

# Drop the id column and keep only num, role, and content
prompts = prompts.drop(columns=["id"])
prompts = prompts.dropna(subset=["content"]).reset_index(drop=True)

# --- LOAD OR CREATE OUTPUT FILE ---
if os.path.exists(OUTPUT_FILE):
    done = pd.read_csv(OUTPUT_FILE)
    completed_nums = set(done["num"])
else:
    done = pd.DataFrame(columns=["num", "role", "content", "response"])
    completed_nums = set()

# --- FILTER UNPROCESSED PROMPTS ---
remaining = prompts[~prompts["num"].isin(completed_nums)].reset_index(drop=True)

if remaining.empty:
    print("✅ All prompts have already been processed!")
else:
    print(f"🧠 Remaining prompts: {len(remaining)}")

    # Select next small batch
    batch = remaining.head(BATCH_SIZE)

    for i, row in batch.iterrows():
        print(f"\n🔹 Processing num={row['num']}: {row['content'][:70]}...")

        try:
            result = ollama.chat(
                model=MODEL,
                messages=[{"role": row["role"], "content": row["content"]}]
            )

            response = result["message"]["content"]

            # Append to results
            new_row = pd.DataFrame([{
                "num": row["num"],
                "role": row["role"],
                "content": row["content"],
                "response": response
            }])

            done = pd.concat([done, new_row], ignore_index=True)

            # Save after each prompt (safe resume)
            done.to_csv(OUTPUT_FILE, index=False)
            time.sleep(2)  # Helps keep CPU/memory stable

        except Exception as e:
            print(f"⚠️ Error on num={row['num']}: {e}")
            break  # Stop gracefully, can resume later

    print(f"\n✅ Finished {len(batch)} prompts. Run again later for the next batch.")
