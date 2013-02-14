package proton;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Basic utilities for templates.
 * 
 * @author Jason R Briggs
 */
public final class Utils {

    private static final Set<String> EXCLUDED_METHODS = toSet("getClass");

    private static Map<Class<?>, List<Method>> methodsMap = new HashMap<Class<?>, List<Method>>();

    /**
     * Return the specified array of names as a Set.
     */
    public static Set<String> toSet(String... names) {
        HashSet<String> set = new HashSet<String>();
        for (int i = 0; i < names.length; i++) {
            set.add(names[i]);
        }
        return set;
    }

    /**
     * Return the list of get methods for a specified class.
     */
    public static List<Method> findGetters(Class<?> clazz) {
        ArrayList<Method> methods = new ArrayList<Method>();
        for (Method method : clazz.getMethods()) {
            if (method.getName().startsWith("get") && !EXCLUDED_METHODS.contains(method.getName()) && method.getParameterTypes().length == 0) {
                methods.add(method);
            }
        }
        return methods;
    }

    /**
     * Return the name of a property (i.e. remove the "get" and lowercase the first letter).
     */
    public static String getPropertyName(Method method) {
        String name = method.getName().substring(3);
        return name.substring(0, 1).toLowerCase() + name.substring(1);
    }

    /**
     * Invoke the getters on an object, return a map keyed by property name.
     */
    public static Map<String, Object> invokeGetters(Object obj) throws IllegalAccessException, InvocationTargetException {
        Class<?> clazz = obj.getClass();
        if (!methodsMap.containsKey(clazz)) {
            methodsMap.put(clazz, findGetters(clazz));
        }

        List<Method> methods = methodsMap.get(clazz);

        Map<String, Object> rtn = new HashMap<String, Object>();
        for (Method method : methods) {
            Object val = method.invoke(obj);
            rtn.put(getPropertyName(method), val);
        }

        return rtn;
    }

}