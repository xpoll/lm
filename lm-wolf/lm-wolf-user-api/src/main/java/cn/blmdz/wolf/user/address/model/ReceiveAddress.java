package cn.blmdz.wolf.user.address.model;

import java.io.Serializable;
import java.util.Date;

public class ReceiveAddress implements Serializable {
   private static final long serialVersionUID = -3108087639454673072L;
   private Long id;
   private Long userId;
   private String receiveUserName;
   private String phone;
   private String mobile;
   private String email;
   private Boolean isDefault;
   private Integer status;
   private String province;
   private Integer provinceId;
   private String city;
   private Integer cityId;
   private String region;
   private Integer regionId;
   private String street;
   private Integer streetId;
   private String detail;
   private String postcode;
   private Date createdAt;
   private Date updatedAt;

   public void setId(Long id) {
      this.id = id;
   }

   public Long getId() {
      return this.id;
   }

   public void setUserId(Long userId) {
      this.userId = userId;
   }

   public Long getUserId() {
      return this.userId;
   }

   public void setReceiveUserName(String receiveUserName) {
      this.receiveUserName = receiveUserName;
   }

   public String getReceiveUserName() {
      return this.receiveUserName;
   }

   public void setPhone(String phone) {
      this.phone = phone;
   }

   public String getPhone() {
      return this.phone;
   }

   public void setMobile(String mobile) {
      this.mobile = mobile;
   }

   public String getMobile() {
      return this.mobile;
   }

   public void setEmail(String email) {
      this.email = email;
   }

   public String getEmail() {
      return this.email;
   }

   public void setIsDefault(Boolean isDefault) {
      this.isDefault = isDefault;
   }

   public Boolean getIsDefault() {
      return this.isDefault;
   }

   public void setStatus(Integer status) {
      this.status = status;
   }

   public Integer getStatus() {
      return this.status;
   }

   public void setProvince(String province) {
      this.province = province;
   }

   public String getProvince() {
      return this.province;
   }

   public void setProvinceId(Integer provinceId) {
      this.provinceId = provinceId;
   }

   public Integer getProvinceId() {
      return this.provinceId;
   }

   public void setCity(String city) {
      this.city = city;
   }

   public String getCity() {
      return this.city;
   }

   public void setCityId(Integer cityId) {
      this.cityId = cityId;
   }

   public Integer getCityId() {
      return this.cityId;
   }

   public void setRegion(String region) {
      this.region = region;
   }

   public String getRegion() {
      return this.region;
   }

   public void setRegionId(Integer regionId) {
      this.regionId = regionId;
   }

   public Integer getRegionId() {
      return this.regionId;
   }

   public void setStreet(String street) {
      this.street = street;
   }

   public String getStreet() {
      return this.street;
   }

   public void setStreetId(Integer streetId) {
      this.streetId = streetId;
   }

   public Integer getStreetId() {
      return this.streetId;
   }

   public void setDetail(String detail) {
      this.detail = detail;
   }

   public String getDetail() {
      return this.detail;
   }

   public void setPostcode(String postcode) {
      this.postcode = postcode;
   }

   public String getPostcode() {
      return this.postcode;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public Date getCreatedAt() {
      return this.createdAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }
}
