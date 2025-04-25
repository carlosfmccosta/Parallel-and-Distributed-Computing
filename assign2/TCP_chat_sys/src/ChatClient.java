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

    public void start_client()
    {
        int maxAttempts = 3;
        int attempt = 0;
        boolean connected = false;
        Socket socket = null;

        while (attempt < maxAttempts && !connected)
        {
            attempt++;

            try
            {
                System.out.println("Attempting to connect to server (Attempt " + attempt + "/" + maxAttempts + ")...");
                socket = new Socket(serverIP, port);
                connected = true;
                System.out.println("Connection established!");

                BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
                Scanner scanner = new Scanner(System.in);

                if (!handleAuthentication(in, out, scanner))
                {
                    System.out.println("Authentication failed. Disconnecting.");
                    return;
                }

                Thread listener = new Thread(() -> {
                    try
                    {
                        String serverMsg;
                        while ((serverMsg = in.readLine()) != null) {
                            System.out.println("\n" + serverMsg);
                            System.out.print("You: ");
                        }
                    }
                    catch (IOException e)
                    {
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
            catch (IOException e)
            {
                System.out.println("Connection attempt " + attempt + " failed: " + e.getMessage());
            }
            finally
            {
                if (!connected)
                {
                    try
                    {
                        socket.close();
                    }
                    catch (IOException e)
                    {
                        //ignore
                    }
                }
            }

            if (!connected && attempt < maxAttempts)
            {
                try
                {
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


    private boolean handleAuthentication(BufferedReader in, PrintWriter out, Scanner scanner) throws IOException
    {
        System.out.println("Do you want to [login] or [register]?");
        System.out.print("Choice: ");
        String mode = scanner.nextLine().trim().toLowerCase();

        while (!mode.equals("login") && !mode.equals("register"))
        {
            System.out.println("Invalid choice. Please enter 'login' or 'register'.");
            System.out.print("Choice: ");

            mode = scanner.nextLine().trim().toLowerCase();
        }

        out.println(mode);

        while (true)
        {
            String serverPrompt = in.readLine();

            if (serverPrompt == null)
            {
                System.out.println("\nServer closed connection.");
                return false;
            }

            if (serverPrompt.startsWith("Enter username:"))
            {
                System.out.print("Username: ");
                out.println(scanner.nextLine());
            }
            else if (serverPrompt.startsWith("Enter password:"))
            {
                System.out.print("Password: ");
                out.println(scanner.nextLine());
            }
            else if (serverPrompt.startsWith("AUTH_SUCCESS"))
            {
                System.out.println(serverPrompt);
                return true;
            }
            else if (serverPrompt.startsWith("AUTH_FAIL"))
            {
                System.out.println(serverPrompt);

                if (serverPrompt.contains("Too many failed"))
                {
                    return false;
                }
            }
            else
            {
                System.out.println("Server: " + serverPrompt);
            }
        }
    }




    public static void main(String[] args) {

        String ip = "localhost";
        int port = 8080;

        ChatClient client = new ChatClient(ip, port);
        client.start_client();
    }
}
