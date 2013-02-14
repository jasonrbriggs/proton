package proton.utils.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Info for the template about a particular property.
 * 
 * @author Jason R Briggs
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface TemplateInfo {

    String rid() default "";
    String eid() default "";
    String aid() default "";
    String attr() default "";
    String[] attrs() default { };
    boolean all() default false;
}
