package proton.utils.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marker annotation for Template model data (i.e. value objects or DTOs).  A way of telling the {@link TemplateHelper}
 * that this class contains data we will be interested in using with the {@link Template}.
 * 
 * @author Jason R Briggs
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface TemplateView {

    /**
     * A suffix to use with ids.  This is so we can define a superclass with one set of ids,
     * then override in a subclass with a different set of ids (for example).
     */
    String suffix() default "";

}
