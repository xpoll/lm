package cn.blmdz.wolf.auth.role;

import java.util.List;

import cn.blmdz.wolf.user.auth.CustomRole;

public interface CustomRoleLoader {
   List<CustomRole> load(List var1) throws CustomRoleLoadException;
}
