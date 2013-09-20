package proton.utils;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import proton.Template;
import proton.utils.annotation.TemplateInfo;
import proton.utils.annotation.TemplateView;

/**
 *
 * @author Jason R Briggs
 */
public class TemplateHelper {

    private HashMap<String, Counter> elementCounters = new HashMap<String, Counter>();
    private HashMap<String, Counter> attributeCounters = new HashMap<String, Counter>();

    private static final HashMap<Class, List<Method>> templateInfoMethodsCache = new HashMap<Class, List<Method>>();
    private static final HashMap<Class, List<Method>> requestInfoMethodsCache = new HashMap<Class, List<Method>>();

    private class Counter {
        private int count = -1;
        public int increment() {
            count++;
            return count;
        }
    }

    public void decorate(Template tmp, Object... models) {
        for (Object model : models) {
            if (model == null || !model.getClass().isAnnotationPresent(TemplateView.class)) {
                continue;
            }

            List<Method> methods = getAnnotatedMethods(model, TemplateInfo.class, templateInfoMethodsCache, true);
            if (methods.isEmpty()) {
                continue;
            }

            TemplateView templateView = model.getClass().getAnnotation(TemplateView.class);
            String suffix = templateView.suffix();

            for (Method m : methods) {
                try {
                    Object value = m.invoke(model);
                    Collection coll = null;
                    if (value instanceof Collection) {
                        coll = (Collection) value;
                    }

                    TemplateInfo templateInfo = m.getAnnotation(TemplateInfo.class);

                    String rid = templateInfo.rid();
                    String eid = templateInfo.eid();
                    String aid = templateInfo.aid();
                    String attr = templateInfo.attr();
                    boolean all = templateInfo.all();

                    if (!isEmpty(rid) && coll != null) {
                        tmp.repeatElement(rid + suffix, coll.size());
                    }

                    if (!isEmpty(eid)) {
                        int idx;
                        if (!all) {
                            Counter counter = getCounter(eid + suffix, elementCounters);
                            idx = counter.increment();
                        }
                        else {
                            idx = -1;
                        }

                        if (value != null) {
                            tmp.setElement(eid + suffix, value, idx);
                        }
                        else {
                            tmp.hideElement(eid + suffix, idx);
                        }
                    }

                    if (!isEmpty(aid) && !isEmpty(attr) && coll == null) {
                        int idx;
                        if (!all) {
                            Counter counter = getCounter(aid + suffix + "-" + attr, attributeCounters);
                            idx = counter.increment();
                        }
                        else {
                            idx = -1;
                        }

                        tmp.setAttribute(aid + suffix, attr, (String) value, idx);
                    }

                    if (coll != null) {
                        for (Object obj : coll) {
                            decorate(tmp, obj);
                        }
                    }
                    else if (value != null && value.getClass().isAnnotationPresent(TemplateView.class)) {
                        decorate(tmp, value);
                    }

                }
                catch (Throwable ex) {
                    ex.printStackTrace();
                }
            }
        }
    }

    private List<Method> getAnnotatedMethods(Object obj, Class annotation, Map<Class, List<Method>> cache, boolean model) {
        Class clazz = obj.getClass();
        if (!cache.containsKey(clazz)) {
            synchronized (cache) {
                ArrayList<Method>annotatedMethods = new ArrayList<Method>();

                Method[] methods;
                if (model) {
                    methods = getMethods(obj, TemplateView.class);
                }
                else {
                    methods = getMethods(obj, null);
                }

                for (Method method : methods) {
                    if (method.isAnnotationPresent(annotation)) {
                        annotatedMethods.add(method);
                    }
                }

                cache.put(clazz, annotatedMethods);
            }
        }

        return cache.get(clazz);
    }

    private Method[] getMethods(Object obj, Class classAnnotation) {
        if (classAnnotation == null || obj.getClass().isAnnotationPresent(TemplateView.class)) {
            return obj.getClass().getMethods();
        }
        for (int i = 0; i < obj.getClass().getGenericInterfaces().length; i++) {
            Class iface = (Class) obj.getClass().getGenericInterfaces()[i];
            if (iface.isAnnotationPresent(TemplateView.class)) {
                return iface.getMethods();
            }
        }
        return null;
    }

    private Counter getCounter(String id, Map<String, Counter> counters) {
        if (!counters.containsKey(id)) {
            counters.put(id, new Counter());
        }
        return counters.get(id);
    }

    private boolean isEmpty(String s) {
        return s == null || s.equals("");
    }
}
