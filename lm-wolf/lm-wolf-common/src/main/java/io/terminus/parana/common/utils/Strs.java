package io.terminus.parana.common.utils;

import com.google.common.base.Optional;
import io.terminus.common.utils.Params;
import javax.annotation.Nullable;

public class Strs {
   public static Optional parseLong(@Nullable Object obj) {
      String str = Params.trimToNull(obj);
      return str == null?Optional.absent():Optional.of(Long.valueOf(Long.parseLong(str)));
   }

   public static Optional parseInt(@Nullable Object obj) {
      String str = Params.trimToNull(obj);
      return str == null?Optional.absent():Optional.of(Integer.valueOf(Integer.parseInt(str)));
   }
}
