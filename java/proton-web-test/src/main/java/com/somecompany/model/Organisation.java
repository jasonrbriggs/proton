package com.somecompany.model;

/**
 *
 * @author Jason R Briggs
 */
public interface Organisation extends Comparable<Organisation> {

    int getId();

    String getName();

    String getOrgUnitId();

    Address getAddress();

    void setName(String name);

}