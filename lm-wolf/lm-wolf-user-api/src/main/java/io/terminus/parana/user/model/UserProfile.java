package io.terminus.parana.user.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;
import io.terminus.common.utils.JsonMapper;
import io.terminus.parana.common.constants.JacksonType;
import java.io.Serializable;
import java.util.Collections;
import java.util.Date;
import java.util.Map;

public class UserProfile implements Serializable {
   private static final long serialVersionUID = 8610043299655883012L;
   private static final ObjectMapper objectMapper = JsonMapper.nonEmptyMapper().getMapper();
   private Long id;
   private Long userId;
   private String birth;
   private String realName;
   private Integer gender;
   private Integer provinceId;
   private String province;
   private Integer cityId;
   private String city;
   private Integer regionId;
   private String region;
   private String street;
   private String avatar;
   private Map extra;
   @JsonIgnore
   private String extraJson;
   private Date createdAt;
   private Date updatedAt;

   public UserProfile(Long userId) {
      this.userId = userId;
   }

   public void setExtraJson(String extraJson) throws Exception {
      this.extraJson = extraJson;
      if(Strings.isNullOrEmpty(extraJson)) {
         this.extra = Collections.emptyMap();
      } else {
         this.extra = (Map)objectMapper.readValue(extraJson, JacksonType.MAP_OF_STRING);
      }

   }

   public String toString() {
      return "UserProfile(id=" + this.getId() + ", userId=" + this.getUserId() + ", birth=" + this.getBirth() + ", realName=" + this.getRealName() + ", gender=" + this.getGender() + ", provinceId=" + this.getProvinceId() + ", province=" + this.getProvince() + ", cityId=" + this.getCityId() + ", city=" + this.getCity() + ", regionId=" + this.getRegionId() + ", region=" + this.getRegion() + ", street=" + this.getStreet() + ", avatar=" + this.getAvatar() + ", extra=" + this.getExtra() + ", extraJson=" + this.getExtraJson() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof UserProfile)) {
         return false;
      } else {
         UserProfile other = (UserProfile)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$userId = this.getUserId();
            Object other$userId = other.getUserId();
            if(this$userId == null) {
               if(other$userId != null) {
                  return false;
               }
            } else if(!this$userId.equals(other$userId)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof UserProfile;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $userId = this.getUserId();
      result = result * 59 + ($userId == null?0:$userId.hashCode());
      return result;
   }

   public UserProfile() {
   }

   public Long getId() {
      return this.id;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public Long getUserId() {
      return this.userId;
   }

   public void setUserId(Long userId) {
      this.userId = userId;
   }

   public String getBirth() {
      return this.birth;
   }

   public void setBirth(String birth) {
      this.birth = birth;
   }

   public String getRealName() {
      return this.realName;
   }

   public void setRealName(String realName) {
      this.realName = realName;
   }

   public Integer getGender() {
      return this.gender;
   }

   public void setGender(Integer gender) {
      this.gender = gender;
   }

   public Integer getProvinceId() {
      return this.provinceId;
   }

   public void setProvinceId(Integer provinceId) {
      this.provinceId = provinceId;
   }

   public String getProvince() {
      return this.province;
   }

   public void setProvince(String province) {
      this.province = province;
   }

   public Integer getCityId() {
      return this.cityId;
   }

   public void setCityId(Integer cityId) {
      this.cityId = cityId;
   }

   public String getCity() {
      return this.city;
   }

   public void setCity(String city) {
      this.city = city;
   }

   public Integer getRegionId() {
      return this.regionId;
   }

   public void setRegionId(Integer regionId) {
      this.regionId = regionId;
   }

   public String getRegion() {
      return this.region;
   }

   public void setRegion(String region) {
      this.region = region;
   }

   public String getStreet() {
      return this.street;
   }

   public void setStreet(String street) {
      this.street = street;
   }

   public String getAvatar() {
      return this.avatar;
   }

   public void setAvatar(String avatar) {
      this.avatar = avatar;
   }

   public Map getExtra() {
      return this.extra;
   }

   public String getExtraJson() {
      return this.extraJson;
   }

   public Date getCreatedAt() {
      return this.createdAt;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }
}
