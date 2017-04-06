package cn.blmdz.wolf.user.model;

import java.io.Serializable;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;

import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.common.constants.JacksonType;

public class User implements Serializable {
   private static final long serialVersionUID = -2961193418926377287L;
   private static final ObjectMapper objectMapper = JsonMapper.nonEmptyMapper().getMapper();
   private Long id;
   private String name;
   private String email;
   private String mobile;
   private String password;
   private Integer type;
   private Integer status;
   private List roles;
   @JsonIgnore
   private String rolesJson;
   private Map extra;
   @JsonIgnore
   private String extraJson;
   private Map tags;
   @JsonIgnore
   private String tagsJson;
   @JsonIgnore
   private Date createdAt;
   @JsonIgnore
   private Date updatedAt;

   public String getTypeName() {
      return null;
   }

   public void setRolesJson(String rolesJson) throws Exception {
      this.rolesJson = rolesJson;
      if(Strings.isNullOrEmpty(rolesJson)) {
         this.roles = Collections.emptyList();
      } else {
         this.roles = (List)objectMapper.readValue(rolesJson, JacksonType.LIST_OF_STRING);
      }

   }

   public void setRoles(List roles) {
      this.roles = roles;
      if(roles != null && !roles.isEmpty()) {
         try {
            this.rolesJson = objectMapper.writeValueAsString(roles);
         } catch (Exception var3) {
            ;
         }
      } else {
         this.rolesJson = null;
      }

   }

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

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof User)) {
         return false;
      } else {
         User other = (User)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$name = this.getName();
            Object other$name = other.getName();
            if(this$name == null) {
               if(other$name != null) {
                  return false;
               }
            } else if(!this$name.equals(other$name)) {
               return false;
            }

            Object this$email = this.getEmail();
            Object other$email = other.getEmail();
            if(this$email == null) {
               if(other$email != null) {
                  return false;
               }
            } else if(!this$email.equals(other$email)) {
               return false;
            }

            Object this$mobile = this.getMobile();
            Object other$mobile = other.getMobile();
            if(this$mobile == null) {
               if(other$mobile != null) {
                  return false;
               }
            } else if(!this$mobile.equals(other$mobile)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof User;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $name = this.getName();
      result = result * 59 + ($name == null?0:$name.hashCode());
      Object $email = this.getEmail();
      result = result * 59 + ($email == null?0:$email.hashCode());
      Object $mobile = this.getMobile();
      result = result * 59 + ($mobile == null?0:$mobile.hashCode());
      return result;
   }

   public String toString() {
      return "User(id=" + this.getId() + ", name=" + this.getName() + ", email=" + this.getEmail() + ", mobile=" + this.getMobile() + ", type=" + this.getType() + ", status=" + this.getStatus() + ", roles=" + this.getRoles() + ")";
   }

   public Long getId() {
      return this.id;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public String getName() {
      return this.name;
   }

   public void setName(String name) {
      this.name = name;
   }

   public String getEmail() {
      return this.email;
   }

   public void setEmail(String email) {
      this.email = email;
   }

   public String getMobile() {
      return this.mobile;
   }

   public void setMobile(String mobile) {
      this.mobile = mobile;
   }

   public String getPassword() {
      return this.password;
   }

   public void setPassword(String password) {
      this.password = password;
   }

   public Integer getType() {
      return this.type;
   }

   public void setType(Integer type) {
      this.type = type;
   }

   public Integer getStatus() {
      return this.status;
   }

   public void setStatus(Integer status) {
      this.status = status;
   }

   public List getRoles() {
      return this.roles;
   }

   public String getRolesJson() {
      return this.rolesJson;
   }

   public Map getExtra() {
      return this.extra;
   }

   public String getExtraJson() {
      return this.extraJson;
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
