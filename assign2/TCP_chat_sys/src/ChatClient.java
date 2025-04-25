import java.io.*;
import java.net.*;
import java.util.Scanner;

public class ChatClient {

    private final String serverIP;
    private final int port;

    public ChatClient(String serverAddress, int port) throws IllegalArgumentException {

        if (serverAddress == null || serverAddress.trim().isEmpty()) {
            throw new IllegalArgumentException("Server IP/hostname cannot be null or empty.");
        }

        if (port < 1 || port > 65535)
        {
            throw new IllegalArgumentException("Server port must be between 1 and 65535.");
        }

        try
        {
            // Checks if IP/hostname is valid
            InetAddress.getByName(serverAddress);
        }
        catch (UnknownHostException e)
        {
            throw new IllegalArgumentException("Invalid server IP/hostname: " + serverAddress);
        }

        this.serverIP = serverAddress;
        this.port = port;
    }

    public void start_client() {

        int maxAttempts = 3;
        int attempt = 0;
        boolean connected = false;
        Socket socket = null;

        while (attempt < maxAttempts && !connected)
        {
            attempt++;
            try {
                System.out.println("Attempting to connect to server (Attempt " + attempt + "/" + maxAttempts + ")...");

                socket = new Socket(serverIP, port);
                connected = true;

                System.out.println("Connection established!");

                try (
                    //reads incoming messages from the server
                    BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));

                    //prints messages from the server
                    PrintWriter out = new PrintWriter(socket.getOutputStream(), true);

                    // reads user input
                    Scanner scanner = new Scanner(System.in)) {

                    Thread listener = new Thread(() -> {
                        try {
                            String serverMsg;
                            while ((serverMsg = in.readLine()) != null) {
                                System.out.println("\n[Server]: " + serverMsg);
                                System.out.print("You: ");
                            }
                        } catch (IOException e) {
                            System.out.println("Connection closed.");
                        }
                    });

                    listener.start();

                    System.out.print("You: ");

                    while (scanner.hasNextLine())
                    {
                        String input = scanner.nextLine();
                        out.println(input);
                    }
                }

            }
            catch (SocketTimeoutException e)
            {
                System.out.println("Connection attempt " + attempt + " timed out.");
            }
            catch (IOException e)
            {
                System.out.println("Connection attempt " + attempt + " failed: " + e.getMessage());
            }

            finally {
                if (!connected && socket != null)
                {
                    try
                    {
                        socket.close();
                    }
                    catch (IOException e) {
                        // Ignore close exception
                    }
                }
            }

            if (!connected && attempt < maxAttempts)
            {
                try
                {
                    // wait 2 secs before tryig again
                    Thread.sleep(2000);
                }
                catch (InterruptedException e)
                {
                    Thread.currentThread().interrupt();
                    return;
                }
            }
        }

        if (!connected)
        {
            System.out.println("Failed to connect after " + maxAttempts + " attempts.");
        }
    }

    public static void main(String[] args) {

        String ip = "localhost";
        int port = 8080;

        ChatClient client = new ChatClient(ip, port);
        client.start_client();
    }




}
