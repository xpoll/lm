package cn.blmdz.wolf.auth.role;

import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Table;

import cn.blmdz.wolf.auth.role.CustomRoleLoader;

public class CustomRoleLoaderRegistry {
   private final Table customRoleLoaderTable = HashBasedTable.create();

   public void register(String appKey, String baseRole, CustomRoleLoader loader) {
      this.customRoleLoaderTable.put(appKey, baseRole, loader);
   }

   public CustomRoleLoader getLoader(String appKey, String baseRole) {
      return (CustomRoleLoader)this.customRoleLoaderTable.get(appKey, baseRole);
   }
}
