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

