
#include "fei.h"

void fei_table_initcapacity(FeiState* state, Table* table, int cap)
{
    (void)state;
    fei_table_initnull(state, table);
    if(cap > 0)
    {
        fei_table_adjustcapacity(state, table, cap);
    }
}

void fei_table_initempty(FeiState* state, Table* table)
{
    fei_table_initcapacity(state, table, 0);
}

void fei_table_initnull(FeiState* state, Table* table)
{
    (void)state;
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void fei_table_destroy(FeiState* state, Table* table)
{
    FREE_ARRAY(state, sizeof(TabEntry), table->entries, table->capacity);
    fei_table_initnull(state, table);
}

TabEntry* fei_table_findentry(FeiState* state, int count, TabEntry* entries, int capacity, ObjString* key)
{
    uint32_t index;
    TabEntry* entry;
    TabEntry* tombstone;
    (void)state;
    (void)count;
    // use modulo to map the key's hash to the code index
    index = key->hash % capacity;
    tombstone = NULL;
    while(true)
    {
        // index is 'inserted' here
        entry = &entries[index];
        if(entry->key == NULL)
        {
            if(fei_value_isnull(entry->value))
            {
                // empty entry
                if(tombstone != NULL)
                {
                    return tombstone;
                }
                return entry;
            }
            else
            {
                if(tombstone == NULL)
                {
                    // can return tombstone bucket as empty and reuse it
                    tombstone = entry;
                }
            }
        }
        // compare them in MEMORY
        if(entry->key == key)
        {
            return entry;
        }
        index = (index + 1) % capacity;
    }
    return NULL;
}

bool fei_table_get(FeiState* state, Table* table, ObjString* key, FeiValue* value)
{
    TabEntry* entry;
    if(table->count == 0)
    {
        return false;
    }
    entry = fei_table_findentry(state, table->count, table->entries, table->capacity, key);
    if(entry == NULL)
    {
        return false;
    }
    if(entry->key == NULL)
    {
        return false;
    }
    *value = entry->value;
    return true;
}

void fei_table_adjustcapacity(FeiState* state, Table* table, int capacity)
{
    int i;
    int actualcap;
    TabEntry* dest;
    TabEntry* entry;
    TabEntry* entries;
    actualcap = capacity+2;
    entries = (TabEntry*)ALLOCATE(state, sizeof(TabEntry), actualcap);
    for(i = 0; i < capacity; i++)
    {
        entries[i].key = NULL;
        entries[i].value = fei_value_makenull(state);
    }
    // do not copy tombstones over when growing
    // NOTE: entries may end up in different buckets
    // with the same hash as it is divided by the modulo; loop below recalculates everything
    //fprintf(stderr, "fei_table_adjustcapacity(%d): table->count=%d table->entries=%p\n", actualcap, table->count, table->entries);
    if((table->count > 0) && (table->entries != NULL))
    {
        table->count = 0;
        // traverse through old array
        for(i = 0; i < table->capacity; i++)
        {
            entry = &table->entries[i];
            if(entry->key == NULL)
            {
                continue;
            }
            // pass in new array
            dest = fei_table_findentry(state, table->count, entries, actualcap, entry->key);
            dest->key = entry->key;
            dest->value = entry->value;
            table->count++;
        }
        FREE_ARRAY(state, sizeof(TabEntry), table->entries, table->capacity);
    }
    table->entries = entries;
    table->capacity = actualcap;
}

// inserting into the table, return false if collision
bool fei_table_set(FeiState* state, Table* table, ObjString* key, FeiValue value)
{
    bool isnewkey;
    int capacity;
    TabEntry* entry;
    // make sure array is big enough
    if((table->count + 1) > (table->capacity * TABLE_MAX_LOAD))
    {
        capacity = (GROW_CAPACITY(table->capacity));
        fei_table_adjustcapacity(state, table, capacity);
    }
    entry = fei_table_findentry(state, table->count, table->entries, table->capacity, key);
    isnewkey = entry->key == NULL;
    if(isnewkey && fei_value_isnull(entry->value))
    {
        // fei_value_isnull for tombstones; treat them as full objects
        table->count++;
    }
    entry->key = key;
    entry->value = value;
    return isnewkey;
}

bool fei_table_delete(FeiState* state, Table* table, ObjString* key)
{
    TabEntry* entry;
    if(table->count == 0)
    {
        return false;
    }
    // find entry
    entry = fei_table_findentry(state, table->count, table->entries, table->capacity, key);
    if(entry->key == NULL)
    {
        return false;
    }
    // place tombstone
    entry->key = NULL;
    //bool(true) as the tombstone
    entry->value = fei_value_makebool(state, true);
    return true;
}

void fei_table_mergefrom(FeiState* state, Table* from, Table* to)
{
    int i;
    TabEntry* entry;
    for(i = 0; i < from->capacity; i++)
    {
        entry = &from->entries[i];
        if(entry->key != NULL)
        {
            fei_table_set(state, to, entry->key, entry->value);
        }
    }
}

// used in VM to find the string
// pass in raw character array
ObjString* fei_table_findstring(FeiState* state, Table* table, const char* chars, int length, uint32_t hash)
{
    uint32_t index;
    TabEntry* entry;
    (void)state;
    if(table->count == 0)
    {
        return NULL;
    }
    index = hash % table->capacity;
    for(;;)
    {
        entry = &table->entries[index];
        if(entry->key == NULL)
        {
            // stop if found empty non-tombstone entry
            if(fei_value_isnull(entry->value))
            {
                // return null if not tombstone(tombstone value is bool(true))
                return NULL;
            }
        }
        else if((entry->key->length == length) && (entry->key->hash == hash) && (memcmp(entry->key->chars, chars, length) == 0))
        {
            // found the entry
            return entry->key;
        }
        index = (index + 1) % table->capacity;
    }
    return NULL;
}

// removing unreachable pointers, used to remove string interns in garbage collection
void fei_table_removeunreachable(FeiState* state, Table* table)
{
    int i;
    TabEntry* entry;
    for(i = 0; i < table->capacity; i++)
    {
        entry = &table->entries[i];
        // remove not marked (string) object pointers
        if(entry->key != NULL && !entry->key->obj.ismarked)
        {
            fei_table_delete(state, table, entry->key);
        }
    }
}

// mark global variables, used in VM for garbage collection
void fei_table_mark(FeiState* state, Table* table)
{
    int i;
    TabEntry* entry;
    for(i = 0; i < table->capacity; i++)
    {
        entry = &table->entries[i];
        // need to mark both the STRING KEYS and the actual value/obj itself
        // mark the string key(ObjString type)
        fei_gcmem_markobject(state, (FeiObject*)entry->key);
        // mark the actual avlue
        fei_gcmem_markvalue(state, entry->value);
    }
}


