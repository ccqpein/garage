import json


def transcribe_audio(client, audio_file_path):
    with open(audio_file_path, "rb") as audio_file:
        # print(f"Transcribing audio from {audio_file_path}...")
        transcription = client.audio.transcriptions.create(
            model="whisper-1", file=audio_file, response_format="text"
        )
        # print("Transcription complete.")
        # print(transcription)
        return json.dumps(
            {
                "content": transcription,
            }
        )
