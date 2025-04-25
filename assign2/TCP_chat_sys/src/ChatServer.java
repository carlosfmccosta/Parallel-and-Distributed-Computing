import java.io.*;
import java.net.*;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;


public class ChatServer {

    private final int port;
    private boolean running;
    private ServerSocket serverSocket;

    private final List<Socket> clientSockets = new CopyOnWriteArrayList<>();
    private final List<PrintWriter> clientWriters = new CopyOnWriteArrayList<>();

    private final ClientAuthSystem clientAuth = new ClientAuthSystem();

    public ChatServer(int port)
    {
        this.port = port;
    }

    public void start_server()
    {
        running = true;

        System.out.println("Chat server starting on port " + port + "...");

        try(ServerSocket serverSocket = new ServerSocket(port))
        {
            this.serverSocket = serverSocket;

            System.out.println("Chat server listening on port " + port);

            while (running)
            {
                Socket clientSocket = serverSocket.accept();
                System.out.println("New Client connected: " + clientSocket.getInetAddress().getHostAddress());

                clientSockets.add(clientSocket);
                PrintWriter writer = new PrintWriter(clientSocket.getOutputStream(), true);
                clientWriters.add(writer);

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

    public void stop_server(){
        running = false;

        try {
            if (serverSocket != null && !serverSocket.isClosed())
            {
                serverSocket.close();
            }
        }
        catch (IOException e)
        {
            System.out.println("Error closing server: " + e.getMessage());
        }

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

    private void handleClient(Socket clientSocket, PrintWriter writer) throws NoSuchAlgorithmException{

        String username = null;

        try (BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream())))
        {
            writer.println("AUTH_REQUEST");

            writer.println("Enter username:");
            username = in.readLine();

            writer.println("Enter password:");
            String password = in.readLine();

            if (!clientAuth.verifyClient(username, password))
            {
                writer.println("AUTH_FAIL Invalid username or password");
                clientSocket.close();
                return;
            }

            writer.println("AUTH_SUCCESS Welcome, " + username + "!");

            broadcast("[Server] " + username + " has joined the chat.", null);

            String line;
            while ((line = in.readLine()) != null)
            {
                System.out.println(username + ": " + line);
                broadcast(username + ": " + line, writer);
            }

        }
        catch (IOException e)
        {
            System.out.println("Client disconnected: " + clientSocket.getInetAddress());
        }
        catch (NoSuchAlgorithmException e)
        {
            writer.println("AUTH_FAIL Server error");
            System.out.println("Hashing algorithm not available: " + e.getMessage());
        }
        finally
        {
            try
            {
                clientSockets.remove(clientSocket);
                clientWriters.remove(writer);
                clientSocket.close();

                if (username != null)
                {
                    broadcast("[Server] " + username + " has left the chat.", null);
                }
            }
            catch (IOException e)
            {
                System.out.println("Error cleaning up client connection: " + e.getMessage());
            }
        }
    }


    private void broadcast(String message, PrintWriter sender)
    {
        for (PrintWriter writer : clientWriters)
        {
            if (writer != sender)
            {
                writer.println(message);
            }
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
            server.clientAuth.registerClient("alice", "password123");
            server.clientAuth.registerClient("bob", "secure456");
            System.out.println("Test users registered: alice, bob");
        } catch (Exception e) {
            System.out.println("Error registering test users: " + e.getMessage());
            return;
        }

        server.start_server();
    }

}
