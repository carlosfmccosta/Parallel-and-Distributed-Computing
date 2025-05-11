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
                System.out.println("Attempting to connect to server...");

                Socket socket = new Socket(serverIp, port);
                BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                PrintWriter out = new PrintWriter(socket.getOutputStream(), true);

                if (!authenticateBot(in, out))
                {
                    System.err.println("Authentication failed. Retrying in 5 seconds...");
                    socket.close();
                    TimeUnit.SECONDS.sleep(5);
                    continue;
                }

                System.out.println("Connected to chat server. Waiting for messages...");

                out.println("/join " + currentRoom);
                System.out.println("Joined room: " + currentRoom);

                Thread.startVirtualThread(() -> {
                    try {
                        String message;
                        while ((message = in.readLine()) != null)
                        {
                            System.out.println("Received message: " + message);

                            if (message.contains("@bot"))
                            {
                                String response = generateAIResponse(message);
                                sendBotResponse(response, out);
                            }
                        }
                    }
                    catch (IOException e)
                    {
                        System.err.println("Error reading from server: " + e.getMessage());
                    }
                });

                break;
            }
            catch (IOException | InterruptedException e)
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

    private boolean waitForAuthRequest(BufferedReader in) throws IOException
    {
        int maxTries = 5;

        while (maxTries-- > 0)
        {
            String serverMessage = in.readLine();
            if (serverMessage == null) return false;
            if ("AUTH_REQUEST".equals(serverMessage)) return true;

            System.out.println("Waiting for AUTH_REQUEST, got instead: " + serverMessage);
        }

        return false;
    }


    private boolean authenticateBot(BufferedReader in, PrintWriter out) throws IOException
    {
        if (!waitForAuthRequest(in))
        {
            System.out.println("Did not receive AUTH_REQUEST from server in time.");
            return false;
        }

        out.println("AI_BOT");
        out.println("bot_password");

        String serverMessage = in.readLine();

        if (serverMessage == null)
        {
            System.out.println("Server closed connection during bot authentication.");
            return false;
        }

        if (serverMessage.startsWith("Enter room to join:") || serverMessage.startsWith("AUTH_SUCCESS"))
        {
            System.out.println("Bot logged in successfully!");


            if (serverMessage.startsWith("Enter room to join:"))
            {
                out.println(currentRoom);  //
                System.out.println("Joined room: " + currentRoom);
            }

            serverMessage = in.readLine();

            if (serverMessage != null)
            {
                System.out.println("Received message: " + serverMessage);
            }

            return true;
        }
        else
        {
            System.out.println("Bot authentication failed: " + serverMessage);
            return false;
        }
    }

    private void sendBotResponse(String response, PrintWriter out)
    {
        System.out.println("[Bot] Sending response to server and room" + currentRoom + " : " + response);
        out.println("[Bot]: " + response);
    }

    private String generateAIResponse(String prompt)
    {
        long now = System.currentTimeMillis();

        if (now - lastResponseTime < RESPONSE_COOLDOWN_MS)
        {
            System.out.println("[Bot] Cooldown active. Sending wait message.");
            return "Please wait a moment before asking again...";
        }

        lastResponseTime = now;

        try
        {
            System.out.println("[Bot] Generating response for prompt: " + prompt);

            List<String> context = getRoomContext();
            String contextPrompt = buildContextPrompt(prompt, context);

            String jsonRequest = String.format("{\"model\":\"%s\",\"prompt\":\"%s\",\"stream\":false}", aiModel, escapeJson(contextPrompt));

            System.out.println("[Bot] JSON request to Ollama:\n" + jsonRequest);

            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(ollamaUrl + "/api/generate"))
                    .header("Content-Type", "application/json")
                    .POST(HttpRequest.BodyPublishers.ofString(jsonRequest))
                    .build();

            HttpResponse<String> response = HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofString());

            if (response.statusCode() != 200)
            {
                System.err.println("[Bot] Ollama API error: " + response.body());
                return "Sorry, I'm having technical difficulties. (API Error)";
            }

            String responseBody = response.body();
            System.out.println("[Bot] Raw API response: " + responseBody);

            int start = responseBody.indexOf("\"response\":\"") + 11;
            int end = responseBody.indexOf("\"", start + 1);

            if (start < 11 || end <= start)
            {
                System.err.println("[Bot] Failed to parse AI response properly.");
                return "I'm not sure how to respond...";
            }

            String aiResponse = responseBody.substring(start + 1, end);
            System.out.println("[Bot] Generated AI Response: " + aiResponse);

            return aiResponse;

        }
        catch (Exception e)
        {
            System.err.println("[Bot] AI Error: " + e.getMessage());
            return "I'm having trouble thinking right now...";
        }
    }

    private List<String> getRoomContext()
    {
        List<String> context = new ArrayList<>();

        try
        {
            File logFile = new File(currentRoom + "_log.txt");

            if (!logFile.exists())
            {
                System.out.println("[Bot] No previous room log found.");
                return context;
            }

            System.out.println("[Bot] Reading recent messages from: " + logFile.getName());

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
                System.out.println("[Bot] Room context for prompt:\n" + context);
            }
        }
        catch (IOException e)
        {
            System.err.println("[Bot] Couldn't read room context: " + e.getMessage());
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

        String finalPrompt = sb.toString();
        System.out.println("[Bot] Final prompt with context:\n" + finalPrompt);

        return finalPrompt;
    }

    private String escapeJson(String input) {
        String escapedInput = input.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");

        System.out.println("[Bot] Escaped JSON input: " + escapedInput);
        return escapedInput;
    }


    public static void main(String[] args) {
        new AIClient("localhost", 8080, "http://localhost:11434", "llama3","general").start();
    }
}