package cn.blmdz.wolf.search.dto;

import java.io.Serializable;

public class SearchedShop implements Serializable {
	private static final long serialVersionUID = 1070246599372805989L;
	private Long id;
	private String name;
	private String phone;
	private String imageUrl;
	private Integer provinceId;
	private String province;
	private Integer cityId;
	private String city;
	private Integer regionId;
	private String region;
	private String street;
	private Integer itemCount;

	public Long getId() {
		return this.id;
	}

	public String getName() {
		return this.name;
	}

	public String getPhone() {
		return this.phone;
	}

	public String getImageUrl() {
		return this.imageUrl;
	}

	public Integer getProvinceId() {
		return this.provinceId;
	}

	public String getProvince() {
		return this.province;
	}

	public Integer getCityId() {
		return this.cityId;
	}

	public String getCity() {
		return this.city;
	}

	public Integer getRegionId() {
		return this.regionId;
	}

	public String getRegion() {
		return this.region;
	}

	public String getStreet() {
		return this.street;
	}

	public Integer getItemCount() {
		return this.itemCount;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setPhone(String phone) {
		this.phone = phone;
	}

	public void setImageUrl(String imageUrl) {
		this.imageUrl = imageUrl;
	}

	public void setProvinceId(Integer provinceId) {
		this.provinceId = provinceId;
	}

	public void setProvince(String province) {
		this.province = province;
	}

	public void setCityId(Integer cityId) {
		this.cityId = cityId;
	}

	public void setCity(String city) {
		this.city = city;
	}

	public void setRegionId(Integer regionId) {
		this.regionId = regionId;
	}

	public void setRegion(String region) {
		this.region = region;
	}

	public void setStreet(String street) {
		this.street = street;
	}

	public void setItemCount(Integer itemCount) {
		this.itemCount = itemCount;
	}

}