import java.io.*;
import java.net.*;
import java.net.http.*;
import java.util.*;
import java.util.concurrent.TimeUnit;

public class AIClient
{
    private final String serverIp;
    private final int port;
    private final String ollamaUrl;
    private final String aiModel;
    private String currentRoom;
    private long lastResponseTime = 0;
    private final long RESPONSE_COOLDOWN_MS = 3000;
    private final int MAX_CONTEXT_LINES = 5;

    public AIClient(String serverIp, int port, String ollamaUrl, String aiModel, String currentRoom)
    {
        this.serverIp = serverIp;
        this.port = port;
        this.ollamaUrl = ollamaUrl;
        this.aiModel = aiModel;
        this.currentRoom = currentRoom;
    }

    public void start()
    {
        while (true)
        {
            try
            {
                Socket socket = new Socket(serverIp, port);
                BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                PrintWriter out = new PrintWriter(socket.getOutputStream(), true);

                out.println("AI_Bot");
                out.println("bot_password");

                System.out.println("Connected to chat server. Waiting for messages...");

                out.println("/join " + currentRoom);

                new Thread(() -> {
                    try
                    {
                        String message;
                        while ((message = in.readLine()) != null)
                        {
                            if (message.startsWith("[Server] You have joined room:"))
                            {
                                currentRoom = message.replace("[Server] You have joined room:", "").trim();
                                continue;
                            }

                            if (message.startsWith("[Room:")) {
                                // Extract room name
                                int roomEndIndex = message.indexOf("]");
                                if (roomEndIndex > 0) {
                                    currentRoom = message.substring(6, roomEndIndex).trim();
                                    String actualMessage = message.substring(roomEndIndex + 1).trim();

                                    if (actualMessage.contains("@bot")) {
                                        String response = generateAIResponse(actualMessage);
                                        if (response != null) {
                                            out.println("[Bot]: " + response);
                                        }
                                    }
                                }
                            } else if (message.contains("@bot")) {
                                String response = generateAIResponse(message);
                                if (response != null) {
                                    out.println("[Bot]: " + response);
                                }
                            }
                        }
                    }
                    catch (IOException e)
                    {
                        System.err.println("Error reading from server: " + e.getMessage());
                    }
                }).start();

                break;

            }
            catch (IOException e)
            {
                System.err.println("Connection failed, retrying in 5 seconds...");
                try
                {
                    TimeUnit.SECONDS.sleep(5);
                }
                catch (InterruptedException ie)
                {
                    Thread.currentThread().interrupt();
                    return;
                }
            }
        }
    }

    private String generateAIResponse(String prompt)
    {
        long now = System.currentTimeMillis();
        if (now - lastResponseTime < RESPONSE_COOLDOWN_MS)
        {
            return "Please wait a moment before asking again...";
        }

        lastResponseTime = now;

        try
        {
            List<String> context = getRoomContext();
            String contextPrompt = buildContextPrompt(prompt, context);

            String jsonRequest = String.format(
                    "{\"model\":\"%s\",\"prompt\":\"%s\",\"stream\":false}",
                    aiModel,
                    escapeJson(contextPrompt)
            );

            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(ollamaUrl + "/api/generate"))
                    .header("Content-Type", "application/json")
                    .POST(HttpRequest.BodyPublishers.ofString(jsonRequest))
                    .build();

            HttpResponse<String> response = HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofString());

            if (response.statusCode() != 200)
            {
                System.err.println("Ollama API error: " + response.body());
                return "Sorry, I'm having technical difficulties. (API Error)";
            }


            String responseBody = response.body();

            int start = responseBody.indexOf("\"response\":\"") + 11;
            int end = responseBody.indexOf("\"", start + 1);

            return responseBody.substring(start + 1, end);

        }
        catch (Exception e)
        {
            System.err.println("AI Error: " + e.getMessage());

            return "I'm having trouble thinking right now...";
        }
    }

    private List<String> getRoomContext()
    {
        List<String> context = new ArrayList<>();

        try
        {
            File logFile = new File(currentRoom + "_log.txt");

            if (!logFile.exists()) return context;

            //read last messages for bot context
            try (BufferedReader reader = new BufferedReader(new FileReader(logFile)))
            {
                Deque<String> lastMessages = new ArrayDeque<>();
                String line;
                while ((line = reader.readLine()) != null)
                {
                    if (!line.contains("[Bot]") && !line.trim().isEmpty())
                    {
                        lastMessages.addLast(line);

                        if (lastMessages.size() > MAX_CONTEXT_LINES)
                        {
                            lastMessages.removeFirst();
                        }
                    }
                }

                context.addAll(lastMessages);
            }
        }
        catch (IOException e)
        {
            System.err.println("Couldn't read room context: " + e.getMessage());
        }

        return context;
    }

    private String buildContextPrompt(String prompt, List<String> context)
    {
        StringBuilder sb = new StringBuilder();

        if (!context.isEmpty())
        {
            sb.append("Recent conversation context:\n");

            for (String msg : context)
            {
                sb.append(msg).append("\n");
            }

            sb.append("\n");
        }

        sb.append("Please respond to this: ").append(prompt.replace("@bot", "").trim());

        return sb.toString();
    }

    private String escapeJson(String input)
    {
        return input.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");
    }

    public static void main(String[] args) {
        new AIClient("localhost", 8080, "http://localhost:11434", "llama3","general").start();
    }
}