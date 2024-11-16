#! /usr/bin/env python
import os
import subprocess
import uuid

from openai import OpenAI


def download_audio_from_youtube(video_url, output_file):
    """
    Downloads audio from a YouTube video using yt-dlp.
    """
    # Construct the yt-dlpcommand
    ytdlp_command = [
        "yt-dlp",
        "--extract-audio",
        "--audio-format",
        "mp3",
        "--audio-quality",
        "0",
        "-o",
        output_file,  # sets the output file format
        video_url,
    ]

    print(f"Downloading audio from {video_url}...")

    # Run the yt-dlp command
    result = subprocess.run(ytdlp_command, capture_output=True, text=True)

    # Check if the download was successful
    if result.returncode != 0:
        print("Failed to download video.")
        print(result.stderr)
        return False

    print(f"Audio saved to: {output_file}")
    return True


def transcribe_audio(openai_client, audio_file_path):
    """
    Transcribes audio to text using OpenAI Whisper API.
    """
    with open(audio_file_path, "rb") as audio_file:
        print(f"Transcribing audio from {audio_file_path}...")
        transcription = openai_client.audio.transcriptions.create(
            model="whisper-1", file=audio_file, response_format="text"
        )
        print("Transcription complete.")
        # print(transcription)
        return transcription


def main():
    # Ask if user needs to download the audio
    download_choice = input("Do you need to download the audio? (y/n): ")
    desktop_path = os.path.expanduser("~/Desktop")
    if download_choice.lower() == "y":
        # Get the video link from the user
        video_url = input("Input video link: ")

        # Generate a random name for the audio file and save it on the Desktop
        random_audio_filename = str(uuid.uuid4())[:8] + ".mp3"

        audio_file_path = os.path.join(desktop_path, random_audio_filename)

        # Download the audio using yt-dlp
        if not download_audio_from_youtube(video_url, audio_file_path):
            return

    else:
        # Prompt user for input audio file instead of downloading
        audio_file_path = input("Input the path to your existing audio file: ")

    # Initialize the OpenAI client
    openai_client = OpenAI()

    # Transcribe the audio
    transcribed_text = transcribe_audio(openai_client, audio_file_path)

    # Generate text filename based on the audio filename (without extension)
    text_filename = os.path.splitext(os.path.basename(audio_file_path))[0] + ".txt"
    text_file_path = os.path.join(desktop_path, text_filename)

    # Save the transcription as a text file
    with open(text_file_path, "w") as text_file:
        print(f"Saving transcription to {text_file_path}...")
        text_file.write(transcribed_text)
        print("File saved successfully.")

    # Delete the temporary cropped audio file
    if os.path.exists(audio_file_path):
        os.remove(audio_file_path)
        print(f"Temporary file {audio_file_path} deleted.")


if __name__ == "__main__":
    main()
