package de.innovationgate.wgpublisher;

/**
 * Thrown when access to an administrative resource is denied
 * because no admin is logged in. This should lead to a redirection
 * to the admin login page.
 */
public class AdminLoginNeededException extends WGAServerException {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public AdminLoginNeededException() {
        super("Administrative resource was requested without available admin login");
    }

}
