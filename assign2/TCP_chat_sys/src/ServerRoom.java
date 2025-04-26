import java.io.*;
import java.net.Socket;
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

    public synchronized void removeClient(Socket socket, PrintWriter writer) {
        try {
            clients.remove(socket);
            writers.remove(writer);

            // If the room is empty, delete the log file associated with this room
            if (clients.isEmpty()) {
                File logFile = new File(name + "_log.txt");
                if (logFile.exists()) {
                    logFile.delete();  // Optionally remove the log file when room is empty
                }
            }
        } catch (Exception e) {
            System.out.println("Error removing client from room " + name + ": " + e.getMessage());
        }
    }



    public synchronized void broadcast(String message, PrintWriter sender) {
        try (PrintWriter pw = new PrintWriter(new FileWriter(name + "_log.txt", true), true)) {
            pw.println(message); // auto-flushes
        } catch (IOException e) {
            System.out.println("Error logging message in room " + name + ": " + e.getMessage());
        }

        for (PrintWriter writer : writers) {
            if (writer != sender) {
                writer.println(message);
            }
        }
    }


    // Method to get the last 5 messages from the log file, excluding messages with "You"
    public synchronized List<String> getLastFiveMessages() {
        List<String> lastMessages = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(name + "_log.txt"))) {
            String line;
            // Read the file into a list of lines
            List<String> lines = new ArrayList<>();
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }

            // Get the last 5 lines
            int start = Math.max(0, lines.size() - MAX_MESSAGE_COUNT);
            for (int i = start; i < lines.size(); i++) {
                String message = lines.get(i);
                // Filter out messages that contain "You"
                if (!message.startsWith("You:")) {
                    lastMessages.add(message);
                }
            }
        } catch (IOException e) {
            System.out.println("Error reading the message log: " + e.getMessage());
        }
        return lastMessages;
    }
}
