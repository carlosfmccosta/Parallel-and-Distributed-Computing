import java.io.*;
import java.nio.file.*;
import java.security.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

public class ClientTokenManager {

    private static final String TOKENS_FILE = "user_tokens.dat";
    private final Map<String, UserToken> tokensByFingerprint = new ConcurrentHashMap<>();
    private final Map<String, Map<String, UserToken>> tokensByUsername = new ConcurrentHashMap<>();
    private final ReentrantLock fileLock = new ReentrantLock();

    public static class UserToken implements Serializable
    {
        private static final long serialVersionUID = 1L;

        private final String username;
        private final String deviceFingerprint;
        private final String defaultRoom;
        private final String tokenValue;
        private final long creationTime;
        private long lastAccessTime;

        public UserToken(String username, String deviceFingerprint, String defaultRoom, String tokenValue)
        {
            this.username = username;
            this.deviceFingerprint = deviceFingerprint;
            this.defaultRoom = defaultRoom;
            this.tokenValue = tokenValue;
            this.creationTime = System.currentTimeMillis();
            this.lastAccessTime = creationTime;
        }

        public String getUsername()
        {
            return username;
        }

        public String getDeviceFingerprint()
        {
            return deviceFingerprint;
        }

        public String getDefaultRoom()
        {
            return defaultRoom;
        }

        public String getTokenValue()
        {
            return tokenValue;
        }

        public long getCreationTime()
        {
            return creationTime;
        }

        public long getLastAccessTime()
        {
            return lastAccessTime;
        }

        public void updateLastAccessTime()
        {
            this.lastAccessTime = System.currentTimeMillis();
        }
    }

    public String generateToken(String username, String deviceFingerprint, String defaultRoom)
    {
        if (username == null || deviceFingerprint == null)
        {
            return null;
        }

        String tokenValue = generateUniqueTokenValue();

        UserToken token = new UserToken(username, deviceFingerprint, defaultRoom, tokenValue);

        tokensByFingerprint.put(deviceFingerprint, token);

        tokensByUsername.computeIfAbsent(username, k -> new ConcurrentHashMap<>()).put(tokenValue, token);

        System.out.println("Generated token for user: " + username + " with fingerprint: " + deviceFingerprint);
        saveTokensToFile();

        return tokenValue;
    }

    private String generateUniqueTokenValue()
    {
        SecureRandom random = new SecureRandom();
        byte[] bytes = new byte[32]; // 256 bits
        random.nextBytes(bytes);
        return Base64.getEncoder().encodeToString(bytes);
    }

    public String findUsernameByTokenAndFingerprint(String tokenValue, String deviceFingerprint)
    {
        UserToken token = tokensByFingerprint.get(deviceFingerprint);

        if (token != null)
        {
            Map<String, UserToken> userTokens = tokensByUsername.get(token.getUsername());

            if (userTokens != null)
            {
                for (UserToken userToken : userTokens.values())
                {
                    if (userToken.getTokenValue().equals(tokenValue))
                    {
                        userToken.updateLastAccessTime();

                        return userToken.getUsername();
                    }
                }
            }
        }

        for (Map.Entry<String, Map<String, UserToken>> entry : tokensByUsername.entrySet())
        {
            Map<String, UserToken> userTokens = entry.getValue();
            UserToken userToken = userTokens.get(tokenValue);

            if (userToken != null)
            {
                userToken.updateLastAccessTime();
                return userToken.getUsername();
            }
        }

        return null;
    }

    public String findUsernameByFingerprint(String deviceFingerprint)
    {
        UserToken token = tokensByFingerprint.get(deviceFingerprint);

        if (token != null)
        {
            token.updateLastAccessTime();
            System.out.println("Found token for fingerprint: " + deviceFingerprint + ", username: " + token.getUsername());
            return token.getUsername();
        }

        return null;
    }

    public void updateDefaultRoom(String username, String deviceFingerprint, String newRoom) {
        UserToken token = tokensByFingerprint.get(deviceFingerprint);

        if (token != null) {
            // Generate a new token with the updated room
            generateToken(username, deviceFingerprint, newRoom);
            saveTokensToFile();
        }
    }

    public String getDefaultRoomForFingerprint(String deviceFingerprint)
    {
        UserToken token = tokensByFingerprint.get(deviceFingerprint);

        if (token != null) {
            return token.getDefaultRoom();
        }

        return "general";
    }

    public void removeTokensForUsername(String username)
    {
        Map<String, UserToken> userTokens = tokensByUsername.remove(username);

        if (userTokens != null) {
            for (UserToken token : userTokens.values()) {
                tokensByFingerprint.remove(token.getDeviceFingerprint());
            }
            saveTokensToFile();
        }
    }

    public void loadTokensFromFile()
    {
        fileLock.lock();

        try {
            File file = new File(TOKENS_FILE);

            if (!file.exists())
            {
                System.out.println("No tokens file found. Starting with empty token set.");
                return;
            }

            try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(file)))
            {
                @SuppressWarnings("unchecked")
                Map<String, UserToken> loadedTokens = (Map<String, UserToken>) ois.readObject();

                tokensByFingerprint.clear();
                tokensByUsername.clear();

                for (UserToken token : loadedTokens.values())
                {
                    tokensByFingerprint.put(token.getDeviceFingerprint(), token);

                    tokensByUsername.computeIfAbsent(token.getUsername(), k -> new ConcurrentHashMap<>()).put(token.getTokenValue(), token);
                }

                System.out.println("Loaded " + loadedTokens.size() + " tokens from file.");
            }
            catch (Exception e)
            {
                System.err.println("Error loading tokens: " + e.getMessage());
                e.printStackTrace();
            }
        }
        finally
        {
            fileLock.unlock();
        }
    }

    public void saveTokensToFile()
    {
        fileLock.lock();

        try
        {
            try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(TOKENS_FILE)))
            {
                oos.writeObject(new HashMap<>(tokensByFingerprint));
                System.out.println("Saved " + tokensByFingerprint.size() + " tokens to file.");
            }
            catch (Exception e)
            {
                System.err.println("Error saving tokens: " + e.getMessage());
                e.printStackTrace();
            }
        }
        finally
        {
            fileLock.unlock();
        }
    }

    public void purgeExpiredTokens(long maxAgeMillis)
    {
        long now = System.currentTimeMillis();
        List<UserToken> tokensToRemove = new ArrayList<>();

        for (UserToken token : tokensByFingerprint.values())
        {
            if (now - token.getLastAccessTime() > maxAgeMillis)
            {
                tokensToRemove.add(token);
            }
        }

        for (UserToken token : tokensToRemove)
        {
            tokensByFingerprint.remove(token.getDeviceFingerprint());

            Map<String, UserToken> userTokens = tokensByUsername.get(token.getUsername());
            if (userTokens != null)
            {
                userTokens.remove(token.getTokenValue());

                if (userTokens.isEmpty())
                {
                    tokensByUsername.remove(token.getUsername());
                }
            }
        }

        if (!tokensToRemove.isEmpty())
        {
            System.out.println("Purged " + tokensToRemove.size() + " expired tokens.");
            saveTokensToFile();
        }
    }
}