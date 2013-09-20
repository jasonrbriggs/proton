package com.somecompany.model;

/**
 *
 * @author Jason R Briggs
 */
public class Organisation implements Comparable<Organisation> {

    private int id;

    private String orgUnitId;

    private String name;

    private Address address;

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getOrgUnitId() {
        return orgUnitId;
    }

    public void setOrgUnitId(String orgUnitId) {
        this.orgUnitId = orgUnitId;
    }

    public Address getAddress() {
        return address;
    }

    public void setAddress(Address address) {
        this.address = address;
    }

    @Override
    public int hashCode() {
        return id;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof Organisation)) {
            return false;
        }
        Organisation that = (Organisation) obj;
        return this.getId() == that.getId();
    }

    @Override
    public int compareTo(Organisation that) {
        return this.getOrgUnitId().compareTo(that.getOrgUnitId());
    }

}