package cn.blmdz.wolf.shop.model;

import java.io.Serializable;
import java.util.Collections;
import java.util.Date;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;

import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.common.constants.JacksonType;

public class Shop implements Serializable {
   private static final long serialVersionUID = 2240906413277397381L;
   private static final ObjectMapper objectMapper = JsonMapper.nonEmptyMapper().getMapper();
   private Long id;
   private String outerId;
   private Long userId;
   private String userName;
   private String name;
   private Integer status;
   private Integer type;
   private String phone;
   private Long businessId;
   private String imageUrl;
   private String address;
   @JsonIgnore
   private String extraJson;
   private Map extra;
   private Map tags;
   private String tagsJson;
   @JsonIgnore
   private Date createdAt;
   @JsonIgnore
   private Date updatedAt;

   public void setExtraJson(String extraJson) throws Exception {
      this.extraJson = extraJson;
      if(Strings.isNullOrEmpty(extraJson)) {
         this.extra = Collections.emptyMap();
      } else {
         this.extra = (Map)objectMapper.readValue(extraJson, JacksonType.MAP_OF_STRING);
      }

   }

   public void setExtra(Map extra) {
      this.extra = extra;
      if(extra != null && !extra.isEmpty()) {
         try {
            this.extraJson = objectMapper.writeValueAsString(extra);
         } catch (Exception var3) {
            ;
         }
      } else {
         this.extraJson = null;
      }

   }

   public void setTagsJson(String tagsJson) throws Exception {
      this.tagsJson = tagsJson;
      if(Strings.isNullOrEmpty(tagsJson)) {
         this.tags = Collections.emptyMap();
      } else {
         this.tags = (Map)objectMapper.readValue(tagsJson, JacksonType.MAP_OF_STRING);
      }

   }

   public void setTags(Map tags) {
      this.tags = tags;
      if(tags != null && !tags.isEmpty()) {
         try {
            this.tagsJson = objectMapper.writeValueAsString(tags);
         } catch (Exception var3) {
            ;
         }
      } else {
         this.tagsJson = null;
      }

   }

   public String toString() {
      return "Shop(id=" + this.getId() + ", outerId=" + this.getOuterId() + ", userId=" + this.getUserId() + ", userName=" + this.getUserName() + ", name=" + this.getName() + ", status=" + this.getStatus() + ", type=" + this.getType() + ", phone=" + this.getPhone() + ", businessId=" + this.getBusinessId() + ", imageUrl=" + this.getImageUrl() + ", address=" + this.getAddress() + ", extraJson=" + this.getExtraJson() + ", extra=" + this.getExtra() + ", tags=" + this.getTags() + ", tagsJson=" + this.getTagsJson() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Shop)) {
         return false;
      } else {
         Shop other = (Shop)o;
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

            Object this$name = this.getName();
            Object other$name = other.getName();
            if(this$name == null) {
               if(other$name != null) {
                  return false;
               }
            } else if(!this$name.equals(other$name)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof Shop;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $userId = this.getUserId();
      result = result * 59 + ($userId == null?0:$userId.hashCode());
      Object $name = this.getName();
      result = result * 59 + ($name == null?0:$name.hashCode());
      return result;
   }

   public Long getId() {
      return this.id;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public String getOuterId() {
      return this.outerId;
   }

   public void setOuterId(String outerId) {
      this.outerId = outerId;
   }

   public Long getUserId() {
      return this.userId;
   }

   public void setUserId(Long userId) {
      this.userId = userId;
   }

   public String getUserName() {
      return this.userName;
   }

   public void setUserName(String userName) {
      this.userName = userName;
   }

   public String getName() {
      return this.name;
   }

   public void setName(String name) {
      this.name = name;
   }

   public Integer getStatus() {
      return this.status;
   }

   public void setStatus(Integer status) {
      this.status = status;
   }

   public Integer getType() {
      return this.type;
   }

   public void setType(Integer type) {
      this.type = type;
   }

   public String getPhone() {
      return this.phone;
   }

   public void setPhone(String phone) {
      this.phone = phone;
   }

   public Long getBusinessId() {
      return this.businessId;
   }

   public void setBusinessId(Long businessId) {
      this.businessId = businessId;
   }

   public String getImageUrl() {
      return this.imageUrl;
   }

   public void setImageUrl(String imageUrl) {
      this.imageUrl = imageUrl;
   }

   public String getAddress() {
      return this.address;
   }

   public void setAddress(String address) {
      this.address = address;
   }

   public String getExtraJson() {
      return this.extraJson;
   }

   public Map getExtra() {
      return this.extra;
   }

   public Map getTags() {
      return this.tags;
   }

   public String getTagsJson() {
      return this.tagsJson;
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
