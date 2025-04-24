import java.io.*;
import java.net.*;

public class ChatServer {

    private final int port;

    public ChatServer(int port) {
        this.port = port;
    }

    public void start_server()
    {
        System.out.println("Chat server starting on port " + port + "...");

        try (ServerSocket serverSocket = new ServerSocket(port))
        {
            while (true)
            {
                Socket clientSocket = serverSocket.accept();
                System.out.println("Client connected: " + clientSocket.getInetAddress());

                Thread.ofVirtual().start(() -> handleClient(clientSocket));
            }
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
    }

    private void handleClient(Socket socket) {
        try (
                BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
        )
        {
            out.println("Welcome to the chat server!");

            String line;

            while ((line = in.readLine()) != null)
            {
                System.out.println("Received: " + line);
                out.println("Echo: " + line);
            }

        }
        catch (IOException e)
        {
            System.out.println("Client disconnected.");
        }
    }

    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Usage: java ChatServer <port>");
            return;
        }

        int port = Integer.parseInt(args[0]);

        ChatServer server = new ChatServer(port);
        server.start_server();
    }
}
