import java.io.*;
import java.net.*;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.List;
import java.util.concurrent.locks.ReentrantLock;

public class ChatServer {

    private final int port;
    private boolean running;
    private ServerSocket serverSocket;

    private final List<Socket> clientSockets = new ArrayList<>();
    private final List<PrintWriter> clientWriters = new ArrayList<>();
    private final Map<String, ServerRoom> serverRooms = new HashMap<>();
    private final ReentrantLock lock = new ReentrantLock();

    private final ClientAuthSystem clientAuth = new ClientAuthSystem();

    private PrintWriter botWriter = null;


    public ChatServer(int port) {
        this.port = port;
    }

    public void start_server()
    {
        running = true;
        System.out.println("Chat server starting on port " + port + "...");

        try (ServerSocket serverSocket = new ServerSocket(port))
        {
            this.serverSocket = serverSocket;
            System.out.println("Chat server listening on port " + port);

            while (running)
            {
                Socket clientSocket = serverSocket.accept();
                System.out.println("New Client connected: " + clientSocket.getInetAddress().getHostAddress());

                PrintWriter writer = new PrintWriter(clientSocket.getOutputStream(), true);

                lock.lock();
                try
                {
                    clientSockets.add(clientSocket);
                    clientWriters.add(writer);
                }
                finally
                {
                    lock.unlock();
                }

                Thread.ofVirtual().start(() -> {
                    try
                    {
                        handleClient(clientSocket, writer);
                    }
                    catch (NoSuchAlgorithmException e)
                    {
                        writer.println("AUTH_FAIL Server error");
                        System.out.println("Hashing algorithm not available: " + e.getMessage());

                        try
                        {
                            clientSocket.close();
                        }
                        catch (IOException ex)
                        {
                            System.out.println("Error closing client socket: " + ex.getMessage());
                        }
                    }
                });
            }
        }
        catch (IOException e)
        {
            if (running)
            {
                System.out.println("Server error: " + e.getMessage());
            }
        }
    }

    public void stop_server()
    {
        running = false;

        try
        {
            if (serverSocket != null && !serverSocket.isClosed())
            {
                serverSocket.close();
            }
        }
        catch (IOException e)
        {
            System.out.println("Error closing server: " + e.getMessage());
        }

        lock.lock();
        try
        {
            for (Socket socket : clientSockets)
            {
                try
                {
                    if (socket != null && !socket.isClosed())
                    {
                        socket.close();
                    }
                }
                catch (IOException e)
                {
                    System.out.println("Error closing client socket: " + e.getMessage());
                }
            }

            clientSockets.clear();
            clientWriters.clear();
        }
        finally
        {
            lock.unlock();
        }
    }

    private void handleClient(Socket clientSocket, PrintWriter writer) throws NoSuchAlgorithmException
    {
        String username = null;

        try (BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream())))
        {
            writer.println("AUTH_REQUEST");

            username = performAuthentication(in, writer);
            if (username == null)
            {
                return;
            }

            String botRoom = null;
            if (username.startsWith("AI_Bot#"))
            {
                botRoom = username.split("#")[1];
                username = "AI_Bot";
                botWriter = writer;
            }

            broadcast("[Server] " + username + " has joined the chat.", null);

            chatLoop(username, in, writer, clientSocket, botRoom);

        }
        catch (IOException e)
        {
            System.out.println("Client disconnected: " + clientSocket.getInetAddress());
        }
        finally
        {
            cleanupConnection(clientSocket, writer, username);
        }
    }

    private String performAuthentication(BufferedReader in, PrintWriter writer) throws IOException, NoSuchAlgorithmException
    {
        String mode = in.readLine();
        if (mode == null) return null;

        if ("AI_BOT".equals(mode))
        {
            String password = in.readLine();
            if ("bot_password".equals(password))
            {
                writer.println("Enter room to join:");

                String botRoom = in.readLine();

                if (botRoom == null || botRoom.trim().isEmpty()) {
                    writer.println("AUTH_FAIL Room name cannot be empty");
                    return null;
                }

                writer.println("AUTH_SUCCESS");
                return "AI_Bot#" + botRoom.trim(); // notice the trick
            }
            writer.println("AUTH_FAIL Invalid bot credentials");
            return null;
        }


        mode = mode.trim().toLowerCase();

        if (mode.equals("register"))
        {
            writer.println("Enter username:");
            String username = in.readLine();

            if (username == null || username.trim().isEmpty())
            {
                writer.println("AUTH_FAIL Username cannot be empty");
                return null;
            }

            if (clientAuth.usernameExists(username))
            {
                writer.println("AUTH_FAIL Username already exists");
                return null;
            }

            writer.println("Enter password:");
            String password = in.readLine();

            if (password == null || password.trim().isEmpty())
            {
                writer.println("AUTH_FAIL Password cannot be empty");
                return null;
            }

            if (clientAuth.registerClient(username, password))
            {
                writer.println("AUTH_SUCCESS Welcome, " + username + "!");

                writer.println("AVAILABLE COMMANDS: /join <room_name> - Join/Create chat room :/leave - Leave room&return to default : /listrooms - List all rooms.");
                writer.flush();

                return username;
            }
            else
            {
                writer.println("AUTH_FAIL Registration failed");
                return null;
            }
        }
        else if (mode.equals("login"))
        {
            for (int attempts = 0; attempts < 3; attempts++)
            {
                writer.println("Enter username:");
                String username = in.readLine();

                writer.println("Enter password:");
                String password = in.readLine();

                if (username == null || password == null)
                {
                    writer.println("AUTH_FAIL Input cannot be null");
                    continue;
                }

                if (!clientAuth.usernameExists(username))
                {
                    writer.println("AUTH_FAIL Username does not exist");
                }
                else if (!clientAuth.verifyClient(username, password))
                {
                    writer.println("AUTH_FAIL Invalid password");
                }
                else
                {
                    writer.println("AUTH_SUCCESS Welcome, " + username + "!");

                    writer.println("AVAILABLE COMMANDS: /join <room_name> - Join/Create chat room :/leave - Leave room&return to default: /listrooms - List all rooms.");
                    writer.flush();

                    return username;
                }
            }

            writer.println("AUTH_FAIL Too many failed login attempts.");
            return null;
        }

        writer.println("AUTH_FAIL Invalid mode (must be 'login' or 'register')");
        return null;
    }

    private void chatLoop(String username, BufferedReader in, PrintWriter writer, Socket clientSocket, String botRoom) throws IOException
    {
        if ("AI_Bot".equals(username))
        {
            String line;
            while ((line = in.readLine()) != null)
            {
                if (line.startsWith("[Bot]:"))
                {
                    if (botRoom != null)
                    {
                        ServerRoom room = getOrCreateRoom(botRoom);
                        room.broadcast(line, writer);
                    }
                }
            }
            return;
        }

        String currentRoomName = "general";
        ServerRoom currentRoom = getOrCreateRoom(currentRoomName);
        addClientToRoom(currentRoomName, clientSocket, writer);

        String line;
        while ((line = in.readLine()) != null)
        {
            if (line.startsWith("/join "))
            {
                String newRoomName = line.substring(6).trim();

                if (!newRoomName.isEmpty() && !newRoomName.equals(currentRoomName))
                {

                    removeClientFromRoom(currentRoomName, clientSocket, writer);


                    currentRoomName = newRoomName;
                    currentRoom = getOrCreateRoom(currentRoomName);
                    addClientToRoom(currentRoomName, clientSocket, writer);


                    writer.println("You have joined room: " + currentRoomName);

                    List<String> lastMessages = currentRoom.getLastFiveMessages();
                    for (String msg : lastMessages)
                    {
                        writer.println(msg);
                    }
                }
            }

            else if (line.contains("@bot"))
            {
                currentRoom.broadcast(username + ": " + line, writer);

                PrintWriter botWriter = findBotWriter();

                if (botWriter != null)
                {
                    botWriter.println(line);
                }

                System.out.println("Sending to bot: " + line);
            }

            else if (line.startsWith("/leave"))
            {
                removeClientFromRoom(currentRoomName, clientSocket, writer);

                currentRoomName = "general";
                currentRoom = getOrCreateRoom(currentRoomName);
                addClientToRoom(currentRoomName, clientSocket, writer);

                writer.println("You have left the room and joined the default room.");
            }
            else if (line.equals("/listrooms"))
            {
                StringBuilder roomList = new StringBuilder("Available rooms: ");

                for (String roomName : serverRooms.keySet())
                {
                    roomList.append(roomName).append(" / ");
                }
                writer.println(roomList.toString().trim());
            }
            else
            {
                System.out.println(username + ": " + line);
                currentRoom.broadcast(username + ": " + line, writer);
            }
        }
    }

    private PrintWriter findBotWriter()
    {
        return botWriter;
    }

    private void cleanupConnection(Socket clientSocket, PrintWriter writer, String username)
    {
        lock.lock();

        try
        {
            clientSockets.remove(clientSocket);
            clientWriters.remove(writer);
        }
        finally
        {
            lock.unlock();
        }

        try
        {
            clientSocket.close();
        }
        catch (IOException e)
        {
            System.out.println("Error closing client socket: " + e.getMessage());
        }

        if (username != null)
        {
            broadcast("[Server] " + username + " has left the chat.", null);
        }
    }

    private void broadcast(String message, PrintWriter sender)
    {
        lock.lock();

        try
        {
            for (PrintWriter writer : clientWriters)
            {
                if (writer != sender)
                {
                    writer.println(message);
                }
            }
        }
        finally
        {
            lock.unlock();
        }
    }

    private ServerRoom getOrCreateRoom(String roomName)
    {
        lock.lock();
        try
        {
            boolean roomExists = serverRooms.containsKey(roomName);

            ServerRoom room = serverRooms.computeIfAbsent(roomName, ServerRoom::new);

            if (!roomExists && !"general".equals(roomName)) // Don't spawn bot for default "general" room
            {
                System.out.println("New room created: " + roomName + ", spawning AI bot...");

                new Thread(() -> {
                    try {
                        AIClient bot = new AIClient(
                                "localhost",
                                8080,
                                "http://localhost:11434",
                                "llama3",
                                roomName
                        );
                        bot.start();
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }).start();
            }

            return room;
        }
        finally
        {
            lock.unlock();
        }
    }


    private void addClientToRoom(String roomName, Socket socket, PrintWriter writer)
    {
        lock.lock();
        try
        {
            ServerRoom room = serverRooms.get(roomName);

            if (room != null)
            {
                room.addClient(socket, writer);
            }
        }
        finally
        {
            lock.unlock();
        }
    }

    private void removeClientFromRoom(String roomName, Socket socket, PrintWriter writer)
    {
        lock.lock();
        try
        {
            ServerRoom room = serverRooms.get(roomName);

            if (room != null)
            {
                room.removeClient(socket, writer);

                if (room.clients.isEmpty())
                {
                    serverRooms.remove(roomName);

                    if (!"general".equals(roomName)) // Don't kill the default general bot
                    {
                        System.out.println("Room " + roomName + " is empty, shutting down its AI bot...");
                        disconnectBot(roomName);
                    }
                }
            }
        }
        finally
        {
            lock.unlock();
        }
    }

    private void disconnectBot(String roomName)
    {
        try (Socket socket = new Socket("localhost", 8080);
             PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
             BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream())))
        {
            // Authenticate as AI_BOT
            out.println("AI_BOT");
            out.println("bot_password");
            out.println(roomName);

            String response = in.readLine();
            if ("AUTH_SUCCESS".equals(response))
            {
                // Tell the bot to shut itself down
                out.println("/shutdown");
            }
        }
        catch (IOException e)
        {
            System.out.println("Failed to disconnect bot for room " + roomName + ": " + e.getMessage());
        }
    }


    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Usage: java ChatServer <port>");
            return;
        }

        int port = Integer.parseInt(args[0]);

        ChatServer server = new ChatServer(port);

        // Pre-register a few test users
        try {
            server.clientAuth.registerClient("charlie", "test123");
            System.out.println("Test users registered: alice, bob");
        } catch (Exception e) {
            System.out.println("Error registering test users: " + e.getMessage());
            return;
        }

        server.start_server();
    }
}
