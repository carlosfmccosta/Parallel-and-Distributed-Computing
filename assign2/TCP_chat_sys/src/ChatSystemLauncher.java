public class ChatSystemLauncher {
    public static void main(String[] args) {
        // Start chat server
        new Thread(() -> {
            ChatServer server = new ChatServer(8080);
            server.start_server();
        }).start();

        // Start AI bot after delay for general room
        new Thread(() -> {
            try {
                Thread.sleep(3000); // Wait for server to start
                AIClient bot = new AIClient("localhost", 8080,
                        "http://localhost:11434", "llama3","general");
                bot.start();
                System.out.println("AI Bot connected");
            } catch (Exception e) {
                e.printStackTrace();
            }
        }).start();
    }
}