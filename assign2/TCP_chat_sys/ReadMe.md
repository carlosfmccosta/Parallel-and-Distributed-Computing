# DISTRIBUTED SYSTEMS ASSIGNMENT - G11

## How to run

### Step 1: Make sure you have Docker installed
First, you need to make sure you have Docker installed. Run the following command:
```bash
docker --version
```

If you don´t have Docker, you will need to install it.

### Step 2: Download the Docker image for Ollama
```bash
docker run -d --name ollama -p 11434:11434 ollama/ollama
```


### Step 3: Start the Ollama container
```bash
docker exec ollama ollama pull llama3
```

### Step 4: Go to the TCP_chat_sys repository
Assuming you are at the base directory of the group's repository, run the following command:
```bash
cd assign2/TCP_chat_sys
```

### Step 5: Launch the Chat System
In the terminal where you want to launch the chat system, run the launcher:
```bash
java -cp out ChatSystemLauncher
```

### Step 6: Join as a Client
In another terminal, you can join the system by running:
```bash
java -cp out ChatClient
```



# Project Overview
## Chat Client
### Overview
This project implements a secure chat client that connects to a chat server over SSL. The client supports user authentication, room management, and persistent device identification.

### Features
- Uses SSL for encrypted communication with the server
- Supports both login and registration
- Maintains authentication tokens for persistent login
- Identifies client devices using a unique fingerprint
- Automatically attempts to reconnect on connection failure

### Implementation Notes
- Written in Java with support for virtual threads
- Thread synchronization via java.util.concurrent.locks
- Secure device identification using SHA-256 hashing
- The client implements a token-based authentication system that eliminates the need to provide credentials on each connection.

## AI Chat Bot Client
### Overview
This project implements an AI chat bot client that connects to a chat server, monitors conversations, and responds to mentions using an Ollama-powered AI model.

### Features
- Secure SSL connection to chat server
- Bot authentication and room management
- AI-powered responses using Ollama API
- Conversation context awareness
- Heartbeat mechanism for reliable connections
- Thread-safe operations using Java locks

### Implementation Notes
- Uses Java virtual threads for efficient concurrency
- Thread synchronization via java.util.concurrent.locks
- Cooldown period of 3 seconds between responses
- 30-second heartbeat interval for connection monitoring