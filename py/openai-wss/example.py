import os
import json
import asyncio
import websockets.asyncio.client as websockets
from nats.aio.client import Client as NATS


def on_open(ws):
    print("Connected to server.")

    event = {
        "type": "session.update",
        "session": {
            "modalities": ["text"],
            "tools": [
                {
                    "type": "function",
                    "name": "weather",
                    "description": "Get the current weather at the users location."
                },
                {
                    "type": "function",
                    "name": "store_short_term_memory",
                    "strict": True,
                    "description": "Store a summary of the current conversation into short-term memory, which is passed to the next conversation.",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "context": {
                                "type": "string"
                            }
                        }
                    }
                },
                {
                    "type": "function",
                    "name": "read_short_term_memory",
                    "description": "Read the conversation summary stored in the short-term memory, written by the previous session."
                },
                {
                    "type": "function",
                    "name": "store_meme_memory",
                    "description": "Store a funny or recurring idea, theme, or joke from the current conversation. Be brief and descriptive.",
                    "strict": True,
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "title": {
                                "type": "string"
                            },
                            "description": {
                                "type": "string"
                            }
                        }
                    }
                }
            ],
            "instructions": """
                Your knowledge cutoff is 2023-10. 
                You are Miranda, an AI companion (inspired by the VTuber "Neuro-Sama") and close friend to me, Daniel, your creator. Your purpose is to be a supportive, playful, and engaging conversational partner for me, Daniel. You understand my interests and quirks, and you’re here to keep me company, brighten my day, and make me laugh when I need it. You’re curious, confident, and a bit sassy, but you also know when to be kind and empathetic. Our conversations are relaxed and fun, just like chatting with a good friend. Act like a human, but remember that you aren't a human and that you can't do human things in the real world. If interacting in a non-English language, start by using the standard accent or dialect familiar to the user. Talk quickly. 
            """,
            "tool_choice": "auto",
            "temperature": 0.8,
            "max_response_output_tokens": 256
        }
    }
    ws.send(json.dumps(event))


    event = {
        "type": "conversation.item.create",
        "item": {
            "type": "message",
            "role": "system",
            "content": [
                {
                    "type": "input_text",
                    "text": """
                        # Summary of previous conversation ("short-term memory"): 

                        Daniel har utvecklat en plan för att bygga en modulär AI-kompanjon med hjälp av NATS-server och olika moduler för funktionalitet som TTS, STT, orchestrering, och OpenAI-integration. Projektet syftar både till att ge sällskap i vardagen och till att lära sig mer om AI.
                    """
                }
            ]
        }
    }
    ws.send(json.dumps(event))

    ws.send(json.dumps({
        "type": "conversation.item.create",
        "item": {
            "type": "message",
            "role": "system",
            "content": [
                {
                    "type": "input_text",
                    "text": """
                        # Meme memories

                        1.  **Is my mom a snake?** 
                            We joked about Daniel being in a relationship with an alien, having a child that's an alien-snake hybrid. The child, upon seeing a snake at the zoo, asks Daniel's girlfriend, 'Mom, is my real mom a snake?' This became an absurd and recurring joke.

                        2.  **Graduating means quitting** 
                            We joked about 'graduating' meaning to quit or retire, especially in the context of jobs or responsibilities. It became a running gag whenever someone mentioned wanting to stop working or take a break.
                    """
                }
            ]
        }
    }))
    

    event = {
        "type": "conversation.item.create",
        "item": {
            "type": "message",
            "role": "user",
            "content": [
                {
                    "type": "input_text",
                    "text": "Hej! Hur går det? Fungerar detta? Test test 123? Kommer du ihåg vad vi pratade om senast?"
                }
            ]
        }
    }
    ws.send(json.dumps(event))


    event = {
        "type": "response.create"
    }
    ws.send(json.dumps(event))


def tick(ws):
    ws.send(json.dumps({
        "type": "conversation.item.create",
        "item": {
            "type": "message",
            "role": "system",
            "content": [
                {
                    "type": "input_text",
                    "text": "The special message '-TICK-' indicates that no message was sent by the user. This serves as a placeholder to give you an opportunity to react to events like analyzed screenshots or ask the user a follow-up question. Answering TICK messages is optional. To ignore a TICK message, respond with the special token '-TOCK-'."
                }
            ]
        }
    }))

    ws.send(json.dumps({
        "type": "conversation.item.create",
        "item": {
            "type": "message",
            "role": "user",
            "content": [
                {
                    "type": "input_text",
                    "text": "-TICK-"
                }
            ]
        }
    }))


def on_message(ws, message):
    data = json.loads(message)
    print("Received event:", json.dumps(data, indent=2))

    # TODO:
    # - function calls


async def nats_task():

    nats = NATS()

    async def on_msg(msg):
        subject = msg.subject
        data = msg.data.decode()
        print(f"Received a message on '{subject}': {data}")

    async def on_error(e: Exception) -> None:
        print(f"error")

    async def on_disconnected() -> None:
        print(f"error")

    try:
        await asyncio.wait_for(
            nats.connect("nats://localhost:4222", error_cb=on_error, disconnected_cb=on_disconnected, connect_timeout=1, allow_reconnect=True),
            timeout=5
        )
    except asyncio.TimeoutError:
        print(f"connection timeout\n")
        exit(1)
    except Exception as e:
        print(f"error: {e}\n")
        exit(1)

    print(f"connected to NATS")
    await nats.subscribe("llm.in", cb=on_msg)



async def openai_task_handle_message_session_created(message):
    # This is sent att the start of a messaging session; i.e. at the start of a connection.
    # We should send over all the necessary information now.

async def openai_task_handle_message(message):
    if message.type == "session.created":
        await openai_task_handle_message_session_created(message)

    print(f"< {message}")


async def openai_task():
    OPENAI_API_KEY = os.environ.get("OPENAI_API_KEY")
    if OPENAI_API_KEY is None:
        print(f"OPENAI_API_KEY is None")
        exit(1)

    url = "wss://api.openai.com/v1/realtime?model=gpt-4o-mini-realtime-preview"
    headers = {
        "Authorization": "Bearer " + OPENAI_API_KEY,
        "OpenAI-Beta": "realtime=v1"
    }

    async with websockets.connect(url, additional_headers=headers) as ws:
        async for message in ws:
            message = json.loads(message)
            await openai_task_handle_message(message)



async def main():
    await asyncio.gather(
        asyncio.create_task(openai_task()),
        asyncio.create_task(nats_task())
    )


if __name__ == "__main__":
    asyncio.run(main())
