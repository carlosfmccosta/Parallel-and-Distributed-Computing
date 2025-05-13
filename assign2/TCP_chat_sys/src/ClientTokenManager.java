import java.io.*;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

public class ClientTokenManager {

    private final Map<String, TokenInfo> tokenMap = new ConcurrentHashMap<>();
    private final long TOKEN_EXPIRY_MS = 86400000;
    private final String TOKEN_FILE = "tokens.ser";

    public static class TokenInfo implements Serializable
    {
        private static final long serialVersionUID = 2L;

        public String username;
        public String deviceFingerprint;
        public String currentRoom;
        public long expiryTime;
        public long creationTime;

        public TokenInfo(String username, String deviceFingerprint, String currentRoom, long expiryTime)
        {
            this.username = username;
            this.deviceFingerprint = deviceFingerprint;
            this.currentRoom = currentRoom;
            this.expiryTime = expiryTime;
            this.creationTime = System.currentTimeMillis();
        }

        public boolean isValid()
        {
            return System.currentTimeMillis() < expiryTime;
        }
    }

    public String findUsernameByFingerprint(String deviceFingerprint) {
        for (TokenInfo info : tokenMap.values()) {
            if (info.deviceFingerprint.equals(deviceFingerprint) && info.isValid()) {
                return info.username;
            }
        }
        return null;
    }

    public String generateToken(String username, String deviceFingerprint, String currentRoom)
    {
        String tokenBase = username + ":" + deviceFingerprint + ":" + System.currentTimeMillis();
        String token;

        try
        {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hash = digest.digest(tokenBase.getBytes());

            StringBuilder hexString = new StringBuilder();

            for (byte b : hash)
            {
                String hex = Integer.toHexString(0xff & b);

                if (hex.length() == 1)
                {
                    hexString.append('0');
                }

                hexString.append(hex);
            }
            token = hexString.toString();
        }
        catch (NoSuchAlgorithmException e)
        {
            token = UUID.randomUUID().toString();
        }

        long expiry = System.currentTimeMillis() + TOKEN_EXPIRY_MS;
        tokenMap.put(token, new TokenInfo(username, deviceFingerprint, currentRoom, expiry));

        return token;
    }

    public TokenInfo validateToken(String token, String deviceFingerprint)
    {
        TokenInfo info = tokenMap.get(token);

        if (info != null && info.isValid())
        {
            if (info.deviceFingerprint.equals(deviceFingerprint))
            {
                return info;
            }
            else
            {
                System.out.println("Token device mismatch: Expected " + info.deviceFingerprint + " but got " + deviceFingerprint);
                tokenMap.remove(token);
                return null;
            }
        }

        tokenMap.remove(token);
        return null;
    }

    public boolean updateUserRoom(String token, String newRoom, String deviceFingerprint)
    {
        TokenInfo info = tokenMap.get(token);

        if (info != null && info.isValid())
        {
            if (info.deviceFingerprint.equals(deviceFingerprint))
            {
                info.currentRoom = newRoom;
                return true;
            }
        }
        return false;
    }

    public String getCurrentRoomByUser(String username, String deviceFingerprint)
    {
        for (TokenInfo info : tokenMap.values())
        {
            if (info.username.equals(username) && info.deviceFingerprint.equals(deviceFingerprint) && info.isValid()) {
                return info.currentRoom;
            }
        }
        return null;
    }

    public void invalidateUserTokens(String username)
    {
        tokenMap.entrySet().removeIf(entry -> entry.getValue().username.equals(username));
    }

    @SuppressWarnings("unchecked")
    public void loadTokensFromFile()
    {
        try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(TOKEN_FILE)))
        {
            Map<String, TokenInfo> savedTokens = (Map<String, TokenInfo>) ois.readObject();

            savedTokens.entrySet().removeIf(entry -> !entry.getValue().isValid());
            tokenMap.clear();
            tokenMap.putAll(savedTokens);

            System.out.println("Loaded " + tokenMap.size() + " valid tokens from file.");
        }
        catch (Exception e)
        {
            System.out.println("No saved tokens found or failed to load: " + e.getMessage());
        }
    }

    public void saveTokensToFile()
    {
        try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(TOKEN_FILE)))
        {
            tokenMap.entrySet().removeIf(entry -> !entry.getValue().isValid());

            oos.writeObject(tokenMap);
            System.out.println("Saved " + tokenMap.size() + " tokens to file.");
        }
        catch (IOException e)
        {
            System.out.println("Failed to save tokens: " + e.getMessage());
        }
    }

    public int getValidTokenCount()
    {
        return (int) tokenMap.values().stream().filter(TokenInfo::isValid)
                .count();
    }

    public void registerShutdownHook() {
        Runtime.getRuntime().addShutdownHook(new Thread(this::saveTokensToFile));
    }
}