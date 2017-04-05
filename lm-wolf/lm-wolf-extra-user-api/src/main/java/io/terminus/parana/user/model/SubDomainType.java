package io.terminus.parana.user.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonFormat.Shape;
import com.google.common.base.Optional;
import javax.annotation.Nullable;

@JsonFormat(
   shape = Shape.NUMBER_INT
)
public enum SubDomainType {
   USER(1),
   SHOP(2),
   COM(3);

   private final int value;

   @JsonValue
   public final int value() {
      return this.value;
   }

   private SubDomainType(int value) {
      this.value = value;
   }

   @JsonCreator
   public static SubDomainType of(int value) {
      for(SubDomainType type : values()) {
         if(type.value == value) {
            return type;
         }
      }

      throw new IllegalArgumentException("Invalid SubDomainType value: " + value);
   }

   public static Optional parse(@Nullable Integer value) {
      if(value == null) {
         return Optional.absent();
      } else {
         int primaryValue = value.intValue();

         for(SubDomainType type : values()) {
            if(type.value == primaryValue) {
               return Optional.of(type);
            }
         }

         return Optional.absent();
      }
   }
}
