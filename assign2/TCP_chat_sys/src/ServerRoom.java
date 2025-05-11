import java.io.*;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class ServerRoom {

    private static final int MAX_MESSAGE_COUNT = 5;
    String name;
    List<Socket> clients = new ArrayList<>();
    List<PrintWriter> writers = new ArrayList<>();

    public ServerRoom(String name)
    {
        this.name = name;
    }

    public String getName()
    {
        return name;
    }

    public synchronized void addClient(Socket socket, PrintWriter writer)
    {
        clients.add(socket);
        writers.add(writer);
    }

    public synchronized boolean isEmpty() {
        return clients.isEmpty();
    }

    public synchronized void removeClient(Socket socket, PrintWriter writer)
    {
        try
        {
            clients.remove(socket);
            writers.remove(writer);

            boolean b = !getName().equals("general");

            if (clients.isEmpty() && b)
            {
                File logFile = new File(name + "_log.txt");
                if (logFile.exists())
                {
                    logFile.delete();
                }
            }
        }
        catch (Exception e)
        {
            System.out.println("Error removing client from room " + name + ": " + e.getMessage());
        }
    }

    public synchronized void broadcast(String message, PrintWriter sender)
    {
        try (PrintWriter pw = new PrintWriter(new FileWriter(name + "_log.txt", true), true))
        {
            pw.println(message);
        }
        catch (IOException e)
        {
            System.out.println("Error logging message in room " + name + ": " + e.getMessage());
        }

        for (PrintWriter writer : writers)
        {
            if (writer != sender)
            {
                writer.println(message);
            }
        }
    }

    public synchronized List<String> getLastFiveMessages()
    {
        List<String> lastMessages = new ArrayList<>();

        Path logFilePath = Paths.get(name + "_log.txt");

        if (!Files.exists(logFilePath))
        {
            System.out.println("Log file does not exist yet: " + logFilePath);
            return lastMessages;
        }

        try (BufferedReader reader = Files.newBufferedReader(logFilePath))
        {
            List<String> lines = new ArrayList<>();
            String line;

            while ((line = reader.readLine()) != null)
            {
                lines.add(line);
            }

            int start = Math.max(0, lines.size() - MAX_MESSAGE_COUNT);

            for (int i = start; i < lines.size(); i++)
            {
                String message = lines.get(i);

                if (!message.startsWith("You:"))
                {
                    lastMessages.add(message);
                }
            }
        }
        catch (IOException e)
        {
            System.out.println("Error reading the message log: " + e.getMessage());
        }
        return lastMessages;
    }
}
