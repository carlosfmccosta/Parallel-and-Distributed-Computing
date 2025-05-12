import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

public class ClientTokenManager {
    private final Map<String, TokenInfo> tokenMap = new ConcurrentHashMap<>();
    private final long TOKEN_EXPIRY_MS = 86400000;

    public class TokenInfo {
        String username;
        String currentRoom;
        long expiryTime;

        public TokenInfo(String username, String currentRoom) {
            this.username = username;
            this.currentRoom = currentRoom;
            this.expiryTime = System.currentTimeMillis() + TOKEN_EXPIRY_MS;
        }

        public boolean isValid() {
            return System.currentTimeMillis() < expiryTime;
        }
    }

    public String generateToken(String username, String currentRoom) {
        String token = UUID.randomUUID().toString();
        tokenMap.put(token, new TokenInfo(username, currentRoom));
        return token;
    }

    public TokenInfo validateToken(String token) {
        TokenInfo info = tokenMap.get(token);
        if (info != null && info.isValid()) {
            return info;
        }
        tokenMap.remove(token); // Clean up expired token
        return null;
    }

    public void updateUserRoom(String token, String newRoom) {
        TokenInfo info = tokenMap.get(token);
        if (info != null) {
            info.currentRoom = newRoom;
        }
    }
}