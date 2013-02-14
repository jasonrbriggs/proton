package proton.utils;

import javax.servlet.http.HttpServletRequest;

/**
 *
 * @author Jason R Briggs
 */
public final class WebUtils {

    public static String getPath(HttpServletRequest request, String... paths) {
        StringBuilder path = new StringBuilder(request.getContextPath());
        path.append(request.getServletPath());
        for (String p : paths) {
            path.append('/').append(p);
        }
        return path.toString();
    }

    public static String getPathInfo(HttpServletRequest request) {
        if (request.getPathInfo() != null && request.getPathInfo().startsWith("/")) {
            return request.getPathInfo().substring(1);
        }
        else {
            return request.getPathInfo();
        }
    }

}
