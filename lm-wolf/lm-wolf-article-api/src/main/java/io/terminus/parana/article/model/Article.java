package io.terminus.parana.article.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;
import io.terminus.common.utils.JsonMapper;
import io.terminus.parana.common.constants.JacksonType;
import java.io.IOException;
import java.io.Serializable;
import java.util.Collections;
import java.util.Date;
import java.util.Map;

public class Article implements Serializable {
   private static final long serialVersionUID = 3244860275392203212L;
   private static final ObjectMapper objectMapper = JsonMapper.nonEmptyMapper().getMapper();
   private Long id;
   private Integer type;
   private Long ownerId;
   private String ownerName;
   private Integer status;
   private String title;
   private String description;
   private String content;
   private String number;
   @JsonIgnore
   private String extraJson;
   private Map extra;
   private String tagsJson;
   private Date createdAt;
   private Date updatedAt;

   public void setExtraJson(String extraJson) {
      this.extraJson = extraJson;
      if(Strings.isNullOrEmpty(extraJson)) {
         this.extra = Collections.EMPTY_MAP;
      } else {
         try {
            this.extra = (Map)objectMapper.readValue(extraJson, JacksonType.MAP_OF_STRING);
         } catch (IOException var3) {
            ;
         }
      }

   }

   public void setExtra(Map extra) {
      if(extra == null) {
         this.extraJson = null;
      } else {
         try {
            this.extraJson = objectMapper.writeValueAsString(extra);
         } catch (JsonProcessingException var3) {
            ;
         }
      }

   }

   public Long getId() {
      return this.id;
   }

   public Integer getType() {
      return this.type;
   }

   public Long getOwnerId() {
      return this.ownerId;
   }

   public String getOwnerName() {
      return this.ownerName;
   }

   public Integer getStatus() {
      return this.status;
   }

   public String getTitle() {
      return this.title;
   }

   public String getDescription() {
      return this.description;
   }

   public String getContent() {
      return this.content;
   }

   public String getNumber() {
      return this.number;
   }

   public String getExtraJson() {
      return this.extraJson;
   }

   public Map getExtra() {
      return this.extra;
   }

   public String getTagsJson() {
      return this.tagsJson;
   }

   public Date getCreatedAt() {
      return this.createdAt;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public void setType(Integer type) {
      this.type = type;
   }

   public void setOwnerId(Long ownerId) {
      this.ownerId = ownerId;
   }

   public void setOwnerName(String ownerName) {
      this.ownerName = ownerName;
   }

   public void setStatus(Integer status) {
      this.status = status;
   }

   public void setTitle(String title) {
      this.title = title;
   }

   public void setDescription(String description) {
      this.description = description;
   }

   public void setContent(String content) {
      this.content = content;
   }

   public void setNumber(String number) {
      this.number = number;
   }

   public void setTagsJson(String tagsJson) {
      this.tagsJson = tagsJson;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Article)) {
         return false;
      } else {
         Article other = (Article)o;
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

            Object this$type = this.getType();
            Object other$type = other.getType();
            if(this$type == null) {
               if(other$type != null) {
                  return false;
               }
            } else if(!this$type.equals(other$type)) {
               return false;
            }

            Object this$ownerId = this.getOwnerId();
            Object other$ownerId = other.getOwnerId();
            if(this$ownerId == null) {
               if(other$ownerId != null) {
                  return false;
               }
            } else if(!this$ownerId.equals(other$ownerId)) {
               return false;
            }

            Object this$ownerName = this.getOwnerName();
            Object other$ownerName = other.getOwnerName();
            if(this$ownerName == null) {
               if(other$ownerName != null) {
                  return false;
               }
            } else if(!this$ownerName.equals(other$ownerName)) {
               return false;
            }

            Object this$status = this.getStatus();
            Object other$status = other.getStatus();
            if(this$status == null) {
               if(other$status != null) {
                  return false;
               }
            } else if(!this$status.equals(other$status)) {
               return false;
            }

            Object this$title = this.getTitle();
            Object other$title = other.getTitle();
            if(this$title == null) {
               if(other$title != null) {
                  return false;
               }
            } else if(!this$title.equals(other$title)) {
               return false;
            }

            Object this$description = this.getDescription();
            Object other$description = other.getDescription();
            if(this$description == null) {
               if(other$description != null) {
                  return false;
               }
            } else if(!this$description.equals(other$description)) {
               return false;
            }

            Object this$content = this.getContent();
            Object other$content = other.getContent();
            if(this$content == null) {
               if(other$content != null) {
                  return false;
               }
            } else if(!this$content.equals(other$content)) {
               return false;
            }

            Object this$number = this.getNumber();
            Object other$number = other.getNumber();
            if(this$number == null) {
               if(other$number != null) {
                  return false;
               }
            } else if(!this$number.equals(other$number)) {
               return false;
            }

            Object this$extraJson = this.getExtraJson();
            Object other$extraJson = other.getExtraJson();
            if(this$extraJson == null) {
               if(other$extraJson != null) {
                  return false;
               }
            } else if(!this$extraJson.equals(other$extraJson)) {
               return false;
            }

            Object this$extra = this.getExtra();
            Object other$extra = other.getExtra();
            if(this$extra == null) {
               if(other$extra != null) {
                  return false;
               }
            } else if(!this$extra.equals(other$extra)) {
               return false;
            }

            Object this$tagsJson = this.getTagsJson();
            Object other$tagsJson = other.getTagsJson();
            if(this$tagsJson == null) {
               if(other$tagsJson != null) {
                  return false;
               }
            } else if(!this$tagsJson.equals(other$tagsJson)) {
               return false;
            }

            Object this$createdAt = this.getCreatedAt();
            Object other$createdAt = other.getCreatedAt();
            if(this$createdAt == null) {
               if(other$createdAt != null) {
                  return false;
               }
            } else if(!this$createdAt.equals(other$createdAt)) {
               return false;
            }

            Object this$updatedAt = this.getUpdatedAt();
            Object other$updatedAt = other.getUpdatedAt();
            if(this$updatedAt == null) {
               if(other$updatedAt != null) {
                  return false;
               }
            } else if(!this$updatedAt.equals(other$updatedAt)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof Article;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $type = this.getType();
      result = result * 31 + ($type == null?0:$type.hashCode());
      Object $ownerId = this.getOwnerId();
      result = result * 31 + ($ownerId == null?0:$ownerId.hashCode());
      Object $ownerName = this.getOwnerName();
      result = result * 31 + ($ownerName == null?0:$ownerName.hashCode());
      Object $status = this.getStatus();
      result = result * 31 + ($status == null?0:$status.hashCode());
      Object $title = this.getTitle();
      result = result * 31 + ($title == null?0:$title.hashCode());
      Object $description = this.getDescription();
      result = result * 31 + ($description == null?0:$description.hashCode());
      Object $content = this.getContent();
      result = result * 31 + ($content == null?0:$content.hashCode());
      Object $number = this.getNumber();
      result = result * 31 + ($number == null?0:$number.hashCode());
      Object $extraJson = this.getExtraJson();
      result = result * 31 + ($extraJson == null?0:$extraJson.hashCode());
      Object $extra = this.getExtra();
      result = result * 31 + ($extra == null?0:$extra.hashCode());
      Object $tagsJson = this.getTagsJson();
      result = result * 31 + ($tagsJson == null?0:$tagsJson.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "Article(id=" + this.getId() + ", type=" + this.getType() + ", ownerId=" + this.getOwnerId() + ", ownerName=" + this.getOwnerName() + ", status=" + this.getStatus() + ", title=" + this.getTitle() + ", description=" + this.getDescription() + ", content=" + this.getContent() + ", number=" + this.getNumber() + ", extraJson=" + this.getExtraJson() + ", extra=" + this.getExtra() + ", tagsJson=" + this.getTagsJson() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
