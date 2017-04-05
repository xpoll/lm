package io.terminus.parana.config.model;

import java.io.Serializable;
import java.util.Date;

public class Config implements Serializable {
   private static final long serialVersionUID = 1L;
   private Long id;
   private Integer bizType;
   private String key;
   private String value;
   private String description;
   private String dataType = "string";
   private String group;
   private Date updatedAt;
   private Date createdAt;

   public Config() {
   }

   public Config(Long id) {
      this.id = id;
   }

   public Long getId() {
      return this.id;
   }

   public Integer getBizType() {
      return this.bizType;
   }

   public String getKey() {
      return this.key;
   }

   public String getValue() {
      return this.value;
   }

   public String getDescription() {
      return this.description;
   }

   public String getDataType() {
      return this.dataType;
   }

   public String getGroup() {
      return this.group;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public Date getCreatedAt() {
      return this.createdAt;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public void setBizType(Integer bizType) {
      this.bizType = bizType;
   }

   public void setKey(String key) {
      this.key = key;
   }

   public void setValue(String value) {
      this.value = value;
   }

   public void setDescription(String description) {
      this.description = description;
   }

   public void setDataType(String dataType) {
      this.dataType = dataType;
   }

   public void setGroup(String group) {
      this.group = group;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Config)) {
         return false;
      } else {
         Config other = (Config)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$id = this.getId();
            Object other$id = other.getId();
            if(this$id == null) {
               if(other$id != null) {
                  return false;
               }
            } else if(!this$id.equals(other$id)) {
               return false;
            }

            Object this$bizType = this.getBizType();
            Object other$bizType = other.getBizType();
            if(this$bizType == null) {
               if(other$bizType != null) {
                  return false;
               }
            } else if(!this$bizType.equals(other$bizType)) {
               return false;
            }

            Object this$key = this.getKey();
            Object other$key = other.getKey();
            if(this$key == null) {
               if(other$key != null) {
                  return false;
               }
            } else if(!this$key.equals(other$key)) {
               return false;
            }

            Object this$value = this.getValue();
            Object other$value = other.getValue();
            if(this$value == null) {
               if(other$value != null) {
                  return false;
               }
            } else if(!this$value.equals(other$value)) {
               return false;
            }

            Object this$dataType = this.getDataType();
            Object other$dataType = other.getDataType();
            if(this$dataType == null) {
               if(other$dataType != null) {
                  return false;
               }
            } else if(!this$dataType.equals(other$dataType)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof Config;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $bizType = this.getBizType();
      result = result * 31 + ($bizType == null?0:$bizType.hashCode());
      Object $key = this.getKey();
      result = result * 31 + ($key == null?0:$key.hashCode());
      Object $value = this.getValue();
      result = result * 31 + ($value == null?0:$value.hashCode());
      Object $dataType = this.getDataType();
      result = result * 31 + ($dataType == null?0:$dataType.hashCode());
      return result;
   }
}
