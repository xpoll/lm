package cn.blmdz.wolf.common.util;

import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.databind.JavaType;

import cn.blmdz.home.common.util.JsonMapper;

public class Json {
   public static final JsonMapper NON_EMPTY = JsonMapper.JSON_NON_EMPTY_MAPPER;
   public static final JavaType List_String = NON_EMPTY.createCollectionType(List.class, new Class[]{String.class});
   public static final JavaType List_Long = NON_EMPTY.createCollectionType(List.class, new Class[]{Long.class});
   public static final JavaType Map_String_Object = NON_EMPTY.createCollectionType(Map.class, new Class[]{String.class, Object.class});
   public static final JavaType Map_String_String = NON_EMPTY.createCollectionType(Map.class, new Class[]{String.class, String.class});
}
