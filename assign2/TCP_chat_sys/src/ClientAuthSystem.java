import java.util.concurrent.ConcurrentHashMap;
import java.util.Map;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Base64;

class ClientAuthSystem
{
    private final Map<String, UserCredentials> registeredUsers = new ConcurrentHashMap<>();

    // Class to store salt and hashed password together
    private static class UserCredentials {
        private final String passwordHash;
        private final String salt;

        public UserCredentials(String passwordHash, String salt) {
            this.passwordHash = passwordHash;
            this.salt = salt;
        }

        public String getPasswordHash() {
            return passwordHash;
        }

        public String getSalt() {
            return salt;
        }
    }


    public void registerClient(String username, String plainTextPassword) throws NoSuchAlgorithmException
    {
        // Generate a random salt
        SecureRandom random = new SecureRandom();
        byte[] saltBytes = new byte[16];
        random.nextBytes(saltBytes);
        String salt = Base64.getEncoder().encodeToString(saltBytes);

        String hashedPassword = hashPassword(plainTextPassword, salt);

        registeredUsers.put(username, new UserCredentials(hashedPassword, salt));
    }


    public boolean verifyClient(String username, String attemptedPassword) throws NoSuchAlgorithmException
    {
        UserCredentials credentials = registeredUsers.get(username);

        if (credentials == null)
        {
            return false;
        }

        String hashedAttempt = hashPassword(attemptedPassword, credentials.getSalt());

        return hashedAttempt.equals(credentials.getPasswordHash());
    }

    private String hashPassword(String password, String salt) throws NoSuchAlgorithmException
    {
        MessageDigest md = MessageDigest.getInstance("SHA-256");
        md.update(salt.getBytes());
        byte[] hashedPassword = md.digest(password.getBytes());
        return Base64.getEncoder().encodeToString(hashedPassword);
    }
}