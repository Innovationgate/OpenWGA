package de.innovationgate.license;

/**
 * Thrown on license errors
 * @author tb
 *
 */
public class LicenseException extends Exception {

    private static final long serialVersionUID = -4034614060753876505L;

    public LicenseException(String msg) {
        super(msg);     
    }

    public LicenseException(String msg, Throwable cause) {
        super(msg, cause);
    }
}
