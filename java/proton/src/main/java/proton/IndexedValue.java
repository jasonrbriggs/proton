package proton;

import java.util.HashMap;

/**
 * Simple destructible value holder.  Stores values by their index position, and then "pops" out
 * values as requested.
 *
 * @author Jason R Briggs
 */
public class IndexedValue {
    private static final String WILDCARD = "*";

    private HashMap<String, Object> values = new HashMap<String, Object>();

    private int idx = 0;

    public IndexedValue() {
    }

    /**
     * Set the specified index to a value.  If the index is less than 1, set the wildcard to this value.
     */
    public void set(Object value, int index) {
        if (index >= 0) {
            values.put(Integer.toString(index), value);
        }
        else {
            values.put(WILDCARD, value);
        }
    }

    /**
     * Return the next value. If a wildcard has been set, return the wildcard value.
     */
    public Object pop() {
        if (values.containsKey(WILDCARD)) {
            return values.get(WILDCARD);
        }
        else {
            String index = Integer.toString(idx);
            idx++;
            if (values.containsKey(index)) {
                return values.get(index);
            }
            else {
                return null;
            }
        }
    }
}