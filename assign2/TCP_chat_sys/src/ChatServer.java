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
    private final ReentrantLock lock = new ReentrantLock();


    private final ClientAuthSystem clientAuth = new ClientAuthSystem();

    public ChatServer(int port)
    {
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
            for (Socket socket : clientSockets) {
                try {
                    if (socket != null && !socket.isClosed()) {
                        socket.close();
                    }
                } catch (IOException e) {
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

            String mode = in.readLine();
            if (mode == null)
            {
                clientSocket.close();
                return;
            }

            mode = mode.trim().toLowerCase();

            if (mode.equals("register"))
            {
                writer.println("Enter username:");
                username = in.readLine();

                if (username == null || username.trim().isEmpty())
                {
                    writer.println("AUTH_FAIL Username cannot be empty");
                    clientSocket.close();
                    return;
                }

                if (clientAuth.usernameExists(username))
                {
                    writer.println("AUTH_FAIL Username already exists");
                    clientSocket.close();
                    return;
                }

                writer.println("Enter password:");
                String password = in.readLine();

                if (password == null || password.trim().isEmpty())
                {
                    writer.println("AUTH_FAIL Password cannot be empty");
                    clientSocket.close();
                    return;
                }

                if (clientAuth.registerClient(username, password))
                {
                    writer.println("AUTH_SUCCESS Registered successfully. Welcome, " + username + "!");
                }
                else
                {
                    writer.println("AUTH_FAIL Registration failed");
                    clientSocket.close();
                    return;
                }
            }

            else if (mode.equals("login"))
            {
                int attempts = 0;
                boolean authenticated = false;

                while (attempts < 3) {
                    writer.println("Enter username:");
                    username = in.readLine();

                    writer.println("Enter password:");
                    String password = in.readLine();

                    if (username == null || password == null)
                    {
                        writer.println("AUTH_FAIL Input cannot be null");
                        attempts++;
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
                        authenticated = true;
                        break;
                    }

                    attempts++;
                }

                if (!authenticated) {
                    writer.println("AUTH_FAIL Too many failed login attempts.");
                    clientSocket.close();
                    return;
                }
            }

            else
            {
                writer.println("AUTH_FAIL Invalid mode (must be 'login' or 'register')");
                clientSocket.close();
                return;
            }

            // Broadcast join message and start chat loop
            broadcast("[Server] " + username + " has joined the chat.", null);

            String line;
            while ((line = in.readLine()) != null) {
                System.out.println(username + ": " + line);
                broadcast(username + ": " + line, writer);
            }

        }
        catch (IOException e)
        {
            System.out.println("Client disconnected: " + clientSocket.getInetAddress());
        }
        finally {
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
    }

    private void broadcast(String message, PrintWriter sender) {

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
